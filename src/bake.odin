package bake

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

printfln_verbose :: proc(fmt_string: string, args: ..any) {
    sb := strings.builder_make(context.allocator)
    if bake_config.verbose {
        fmt.sbprintf(&sb, cli.COLOR_VERBOSE + "[VERBOSE] ")
        fmt.sbprintf(&sb, fmt_string, ..args)
        fmt.sbprintf(&sb, cli.COLOR_RESET + "\n")
    }
    if cli.config.stdout_color {
        fmt.printf(strings.to_string(sb))
    } else {
        fmt.printf(cli.strip_color(strings.to_string(sb)))
    }
}

cli_recolor_string :: proc(s: string) -> string {
    sb := strings.builder_make(context.allocator)
    spc := true
    arg := false
    m := false
    for i in 0 ..< len(s) {
        if s[i] != '-' {
            spc = false
        }
        if s[i] == ' ' || s[i] == '\n' {
            spc = true
        }
        if arg && !(('a' <= s[i] && s[i] <= 'z') || ('A' <= s[i] && s[i] <= 'Z') || s[i] == '-') {
            arg = false
            strings.write_string(&sb, cli.COLOR_RESET)
        }
        switch s[i] {
        case '-':
            if !arg && spc {
                arg = true
                strings.write_string(&sb, cli.COLOR_ARG)
                strings.write_byte(&sb, '-')
            } else {
                strings.write_byte(&sb, '-')
            }
        case '`':
            if !m {
                strings.write_byte(&sb, '`')
                strings.write_string(&sb, cli.COLOR_CODE)
            } else {
                strings.write_string(&sb, cli.COLOR_RESET)
                strings.write_byte(&sb, '`')
            }
            m = !m
        case '\'':
            if !m {
                strings.write_byte(&sb, '\'')
                strings.write_string(&sb, cli.COLOR_USER)
            } else {
                strings.write_string(&sb, cli.COLOR_RESET)
                strings.write_byte(&sb, '\'')
            }
            m = !m
        case '[':
            strings.write_byte(&sb, '[')
            strings.write_string(&sb, cli.COLOR_ARG)
        case ']':
            strings.write_string(&sb, cli.COLOR_RESET)
            strings.write_byte(&sb, ']')
        case '<':
            strings.write_byte(&sb, '<')
            strings.write_string(&sb, cli.COLOR_PAR)
        case '>':
            strings.write_string(&sb, cli.COLOR_RESET)
            strings.write_byte(&sb, '>')
        case:
            strings.write_byte(&sb, s[i])
        }
    }
    return strings.to_string(sb)
}

printfln_error :: proc(fmt_string: string, args: ..any) {
    sb := strings.builder_make(context.allocator)
    if cli.config.stderr_color {
        fmt.sbprintf(&sb, cli.COLOR_ERROR + "[ERROR] ")
        fmt.sbprintf(&sb, fmt_string, ..args)
        fmt.sbprintf(&sb, cli.COLOR_RESET + "\n")
    } else {
        fmt.sbprintf(&sb, fmt_string, ..args)
    }
    fmt.eprint(strings.to_string(sb))
}

printfln_color :: proc(fmt_string: string, args: ..any) {
    sb := strings.builder_make(context.allocator)
    fmt.sbprintf(&sb, fmt_string, ..args)
    fmt.sbprintf(&sb, "\n")
    if cli.config.stdout_color {
        fmt.print(cli_recolor_string(strings.to_string(sb)))
    } else {
        fmt.print(strings.to_string(sb))
    }
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
            printfln_error("Invalid option syntax: '%s'.", err.arg)
        case .Opt_No_Value:
            printfln_error("Option '%s' requires a value.", err.opt_name)
        case .Opt_Unknown:
            printfln_error("Unknown option '%s'.", err.opt_name)
        case .Opt_Already_Specified:
            printfln_error("Option '%s' is specified multiple times.", err.opt_name)
        case .Opt_Invalid_Value:
            printfln_error("Option '%s' has invalid value.", err.opt_name)
        }
        printfln_color("* Run `bake -help` to get more details about the command line syntax.")
        os.exit(2)
    }
    if cl.opts["-colors"].(bool) {
        cli.config.stderr_color = true
        cli.config.stdout_color = true
    }
    if cl.opts["-help"].(bool) {
        if cli.config.stdout_color {
            fmt.println(cli_recolor_string(HELP_MESSAGE))
        } else {
            fmt.println(HELP_MESSAGE)
        }
        os.exit(2)
    }
    if cl.opts["-verbose"].(bool) {
        bake_config.verbose = true
    }
    if "-bakefile" in cl.opts {
        bake_config.bakefile_path = cl.opts["-bakefile"].(string)
    }
    if "-cache" in cl.opts {
        bake_config.bake_cache_path = cl.opts["-cache"].(string)
    }
    if len(cl.args) == 0 {
        bake_config.bakefile_target = "build"
    } else if len(cl.args) == 1 {
        bake_config.bakefile_target = cl.args[0]
    } else {
        printfln_error("Unexpected '%s', after [target] was already specified.", cl.args[1])
        printfln_color("* Note: '%s' was specified as [target] earlier", cl.args[0])
        printfln_color("* Note: Only one target is allowed to be executed at once")
        printfln_color("* Run `bake -help` to get more details about the command line syntax.")
        os.exit(2)
    }
    printfln_verbose("Bakefile Target: '%s'", bake_config.bakefile_target)
    if len(bake_config.bakefile_path) == 0 {
        bake_config.bakefile_path = find_bakefile()
        if len(bake_config.bakefile_path) == 0 {
            printfln_error("Bakefile not found at path: '%s'.", bake_config.bakefile_path)
            os.exit(1)
        }
    }
    printfln_verbose("Bakefile Path: '%s'", bake_config.bakefile_path)
    bakefile_bytes, bakefile_bytes_err := os.read_entire_file(bake_config.bakefile_path, context.temp_allocator)
    if bakefile_bytes_err != nil {
        printfln_error("Failed to open bakefile: '%s'\n", bake_config.bakefile_path)
        os.exit(1)
    }
    init_builtins()
    parser := parser_make(bake_config.bakefile_path, transmute(string) bakefile_bytes)
    stmts := parse_stmts(&parser)
    printfln_verbose("Running the bake script...")
    ctx := exec_stmts(bake_config.bakefile_path, transmute(string) bakefile_bytes, stmts)
    mb_value := env_get(ctx.global_env, bake_config.bakefile_target)
    printfln_verbose("Finding the target function '%s'...", bake_config.bakefile_target)
    if value, ok := mb_value.?; !ok {
        printfln_error("Target function '%s' not declared in the script.", bake_config.bakefile_target)
        printfln_color("* Please declare function as `func 'cli' %s () {{}}`", bake_config.bakefile_target)
        os.exit(1)
    } else if func, ok := value.(Bake_Func); !ok {
        printfln_error("Target '%s' is not a function.", bake_config.bakefile_target)
        printfln_color("* Please declare function as `func 'cli' %s () {{}}`", bake_config.bakefile_target)
        os.exit(1)
    } else {
        printfln_verbose("Executing the target function '%s'...", bake_config.bakefile_target)
        cli_args := make([dynamic]Value)
        for arg in cl.targs[:] {
            append(&cli_args, arg)
        }
        ret, err := call_func(&ctx, value.(Bake_Func), []Value{cli_args[:]})
        if tok, ok := err.(Call_Func_Err_CF_Token); ok && tok.tok != nil {
            printfln_error(
                "Control flow token '%s' used in the global scope.",
                tok.tok == .Break?    "break"    :
                tok.tok == .Continue? "continue" :
                tok.tok == .Return?   "return"   :
                "",
            )
            os.exit(1)
        } else if _, ok := err.(Call_Func_Err_Param_Mismatch); ok {
            printfln_error("Target function '%s' has incorrect signature.", bake_config.bakefile_target)
            printfln_color("* Note: The correct signature is `func 'cli' %s(args) {}`", bake_config.bakefile_target)
            os.exit(1)
        }
    }
}
