package bake

import "core:log"
import "core:io"
import "core:strings"
import "core:fmt"
import "core:os"

import "cli"

HELP_MESSAGE :: `bake - Build system

Syntax:
  bake [options..] [target] -- [args..]

* [options..]: Options to run bake. Each of the allowed options is documented
  below.
* [target]: Name of the target function to run inside the bakefile. If empty,
  this the 'build' function is ran by default.
* [args..]: Arguments to pass to the target function specified in the [target]
  parameter.

Available options:
  -v, -verbose      Print verbose logs.
  -bakefile:<path>  Specify an alternative path to bakefile.
                    default: './bakefile'
  -cache:<path>     Specify an alternative path to bake cache.
                    default: './.bake-cache'
  -colors           When the output of bake is redirected, bake doesn't output
                    any ansi colors. Use this option to force using ANSI colors,
                    even when the output is redirected.
`

Bake_Config :: struct {
    verbose: bool,
    bakefile_path: string,
    bakefile_target: string,
    bake_cache_path: string,
}

bake_config := Bake_Config {
    verbose = false,
    bakefile_path = "",
    bakefile_target = "",
    bake_cache_path = ".bake-cache",
}

main :: proc() {
    cl_schema := cli.Schema {
        opts = []cli.Opt_Schema {
            cli.opt_schema(.Boolean, "-help", {"-h", "--help"}),
            cli.opt_schema(.Boolean, "-verbose", {"-v"}),
            cli.opt_schema(.Boolean, "-colors"),
            cli.opt_schema(.String,  "-bakefile"),
            cli.opt_schema(.String,  "-cache"),
        }
    }
    cli.init()
    cl, err := cli.parse_args(cl_schema, os.args)
    if err.kind != nil {
        switch err.kind {
        case .None:
        case .Arg_Missing:
        case .Arg_Unexpected:
        case .Opt_No_Name:
            log.errorf("Invalid option syntax: '%s'.", err.arg)
        case .Opt_No_Value:
            log.errorf("Option '%s' requires a value.", err.opt_name)
        case .Opt_Unknown:
            log.errorf("Unknown option '%s'.", err.opt_name)
        case .Opt_Already_Specified:
            log.errorf("Option '%s' is specified multiple times.", err.opt_name)
        case .Opt_Invalid_Value:
            log.errorf("Option '%s' has invalid value.", err.opt_name)
        }
        log.infof("* Run `bake -help` to get more details about the command line syntax.")
        os.exit(2)
    }
    logger_level := log.Level.Info
    logger_options := log.Options { .Level }
    if cl.opts["-colors"].(bool) {
        cli.config.stderr_color = true
        cli.config.stdout_color = true
        logger_options += { .Terminal_Color }
    }
    if cl.opts["-help"].(bool) {
        if cli.config.stdout_color {
            fmt.println(cli.auto_color(HELP_MESSAGE))
        } else {
            fmt.println(HELP_MESSAGE)
        }
        os.exit(2)
    }
    if cl.opts["-verbose"].(bool) {
        bake_config.verbose = true
        logger_level = .Debug
    }
    if "-bakefile" in cl.opts {
        bake_config.bakefile_path = cl.opts["-bakefile"].(string)
    }
    if "-cache" in cl.opts {
        bake_config.bake_cache_path = cl.opts["-cache"].(string)
    }
    context.logger = bake_logger(logger_level, logger_options)
    log.debugf("Logger initialized.")
    if len(cl.args) == 0 {
        bake_config.bakefile_target = "build"
    } else if len(cl.args) == 1 {
        bake_config.bakefile_target = cl.args[0]
    } else {
        log.errorf("Unexpected '%s', after [target] was already specified.", cl.args[1])
        log.infof("* Note: '%s' was specified as [target] earlier", cl.args[0])
        log.infof("* Note: Only one target is allowed to be executed at once")
        log.infof("* Run `bake -help` to get more details about the command line syntax.")
        os.exit(2)
    }
    log.debugf("Bakefile Target: '%s'", bake_config.bakefile_target)
    if len(bake_config.bakefile_path) == 0 {
        bake_config.bakefile_path = find_bakefile()
        if len(bake_config.bakefile_path) == 0 {
            log.errorf("Bakefile not found at path: '%s'.", bake_config.bakefile_path)
            os.exit(1)
        }
    }
    log.debugf("Bakefile Path: '%s'", bake_config.bakefile_path)
    log.debugf("Bake Cache: '%s'", bake_config.bake_cache_path)
    bakefile_bytes, bakefile_bytes_err := os.read_entire_file(bake_config.bakefile_path, context.temp_allocator)
    if bakefile_bytes_err != nil {
        log.errorf("Failed to open bakefile: '%s'\n", bake_config.bakefile_path)
        os.exit(1)
    }
    init_builtins()
    parser := parser_make(bake_config.bakefile_path, transmute(string) bakefile_bytes)
    stmts := parse_stmts(&parser)
    log.debugf("Running the bake script...")
    ctx := exec_stmts(bake_config.bakefile_path, transmute(string) bakefile_bytes, stmts)
    mb_value := env_get(ctx.global_env, bake_config.bakefile_target)
    log.debugf("Finding the target function '%s'...", bake_config.bakefile_target)
    if value, ok := mb_value.?; !ok {
        log.errorf("Target function '%s' not declared in the script.", bake_config.bakefile_target)
        log.infof("* Please declare function as `func 'cli' %s () {{}}`", bake_config.bakefile_target)
        os.exit(1)
    } else if func, ok := value.(Bake_Func); !ok {
        log.errorf("Target '%s' is not a function.", bake_config.bakefile_target)
        log.infof("* Please declare function as `func 'cli' %s () {{}}`", bake_config.bakefile_target)
        os.exit(1)
    } else {
        log.debugf("Executing the target function '%s'...", bake_config.bakefile_target)
        cli_args := make([dynamic]Value)
        for arg in cl.targs[:] {
            append(&cli_args, arg)
        }
        ret, err := call_func(&ctx, value.(Bake_Func), []Value{cli_args[:]})
        if tok, ok := err.(Call_Func_Err_CF_Token); ok && tok.tok != nil {
            log.errorf(
                "Control flow token '%s' used in the global scope.",
                tok.tok == .Break?    "break"    :
                tok.tok == .Continue? "continue" :
                tok.tok == .Return?   "return"   :
                "",
            )
            os.exit(1)
        } else if _, ok := err.(Call_Func_Err_Param_Mismatch); ok {
            log.errorf("Target function '%s' has incorrect signature.", bake_config.bakefile_target)
            log.infof("* Note: The correct signature is `func 'cli' %s(args) {}`", bake_config.bakefile_target)
            os.exit(1)
        }
    }
}
