package bake

import "core:strings"
import "core:fmt"
import "core:os"

CLI_RESET :: "\e[0m"
CLI_COLOR_VERBOSE :: "\e[38;5;234m"
CLI_COLOR_ERROR :: "\e[38;5;197m"
CLI_COLOR_USER :: "\e[38;5;121m"
CLI_COLOR_CODE :: "\e[38;5;104m"
CLI_COLOR_ARG :: "\e[38;5;220m"
CLI_COLOR_PAR :: "\e[38;5;185m"

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
        fmt.sbprintf(&sb, CLI_COLOR_VERBOSE + "[VERBOSE] ")
        fmt.sbprintf(&sb, fmt_string, ..args)
        fmt.sbprintf(&sb, CLI_RESET + "\n")
    }
    fmt.printf(strings.to_string(sb))
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
        if !(('a' <= s[i] && s[i] <= 'z') || ('A' <= s[i] && s[i] <= 'Z') || s[i] == '-') {
            arg = false
            strings.write_string(&sb, CLI_RESET)
        }
        switch s[i] {
        case '-':
            if !arg && spc {
                arg = true
                strings.write_string(&sb, CLI_COLOR_ARG)
                strings.write_byte(&sb, '-')
            } else {
                strings.write_byte(&sb, '-')
            }
        case '`':
            if !m {
                strings.write_byte(&sb, '`')
                strings.write_string(&sb, CLI_COLOR_CODE)
            } else {
                strings.write_string(&sb, CLI_RESET)
                strings.write_byte(&sb, '`')
            }
            m = !m
        case '\'':
            if !m {
                strings.write_byte(&sb, '\'')
                strings.write_string(&sb, CLI_COLOR_USER)
            } else {
                strings.write_string(&sb, CLI_RESET)
                strings.write_byte(&sb, '\'')
            }
            m = !m
        case '[':
            strings.write_byte(&sb, '[')
            strings.write_string(&sb, CLI_COLOR_ARG)
        case ']':
            strings.write_string(&sb, CLI_RESET)
            strings.write_byte(&sb, ']')
        case '<':
            strings.write_byte(&sb, '<')
            strings.write_string(&sb, CLI_COLOR_PAR)
        case '>':
            strings.write_string(&sb, CLI_RESET)
            strings.write_byte(&sb, '>')
        case:
            strings.write_byte(&sb, s[i])
        }
    }
    return strings.to_string(sb)
}

printfln_error :: proc(fmt_string: string, args: ..any) {
    sb := strings.builder_make(context.allocator)
    fmt.sbprintf(&sb, CLI_COLOR_ERROR + "[ERROR] ")
    fmt.sbprintf(&sb, fmt_string, ..args)
    fmt.sbprintf(&sb, CLI_RESET + "\n")
    fmt.eprintf(strings.to_string(sb))
}

printfln_color :: proc(fmt_string: string, args: ..any) {
    sb := strings.builder_make(context.allocator)
    fmt.sbprintf(&sb, fmt_string, ..args)
    fmt.sbprintf(&sb, "\n")
    fmt.printf(cli_recolor_string(strings.to_string(sb)))
}

main :: proc() {
    extra_args_idx := Maybe(int) {}
    for arg, arg_idx in os.args[1:] {
        if arg == "-h" || arg == "--help" || arg == "-help" {
            fmt.println(cli_recolor_string(HELP_MESSAGE))
            os.exit(2)
        } else if arg == "-verbose" || arg == "-v" {
            bake_config.verbose = true
        } else if arg == "--" {
            extra_args_idx = arg_idx+1
        } else if strings.has_prefix(arg, "-bakefile") {
            if len(arg) < len("-bakefile:") {
                printfln_error("Invalid syntax for `-bakefile` option.")
                printfln_color("* Note: Use -bakefile:<path> syntax to specify the bakefile path.")
                printfln_color("* Run `bake -help` to get more details about the command line syntax.")
                os.exit(2)
            }
            bake_config.bakefile_path = arg[len("-bakefile:"):]
            if len(bake_config.bakefile_path) == 0 {
                printfln_error("Bakefile path is empty.")
                printfln_color("* Note: Use `-bakefile:<path>` syntax to specify the bakefile path.")
                printfln_color("* Run `bake -help` to get more details about the command line syntax.")
                os.exit(2)
            }
        } else if strings.has_prefix(arg, "-cache") {
            if len(arg) < len("-cache") {
                printfln_error("Invalid syntax for `-cache` option.")
                printfln_color("* Note: Use `-cache:<path>` syntax to specify the bakefile path.")
                printfln_color("* Run `bake -help` to get more details about the command line syntax.")
                os.exit(2)
            }
            bake_config.bakefile_path = arg[len("-bakefile:"):]
            if len(bake_config.bakefile_path) == 0 {
                printfln_error("Bakefile cache path is empty.")
                printfln_color("* Note: Use `-cache:<path>` syntax to specify the bakefile path.")
                printfln_color("* Run `bake -help` to get more details about the command line syntax.")
                os.exit(2)
            }
        } else if arg[0] != '-' {
            if extra_args_idx == nil {
                if bake_config.bakefile_target == "" {
                    bake_config.bakefile_target = arg
                } else {
                    printfln_error("Unexpected '%s', after [target] was already specified.", arg)
                    printfln_color("* Note: '%s' was specified as [target] earlier", bake_config.bakefile_target)
                    printfln_color("* Run `bake -help` to get more details about the command line syntax.")
                    os.exit(2)
                }
            }
        } else {
            printfln_error("Unrecognized option '%s'.", arg)
            printfln_color("* Run `bake -help` to get more details about the command line syntax.")
            os.exit(2)
        }
    }
    if bake_config.bakefile_target == "" {
        bake_config.bakefile_target = "build"
    }
    printfln_verbose("Bakefile Target: '%s'", bake_config.bakefile_target)
    extra_args := make([dynamic]string)
    if start_idx, ok := extra_args_idx.?; ok {
        for arg, arg_idx in os.args[start_idx+1:] {
            append(&extra_args, arg)
        }
    }
    if len(bake_config.bakefile_path) == 0 {
        bake_config.bakefile_path = find_bakefile()
        if len(bake_config.bakefile_path) == 0 {
            printfln_error("Bakefile not found at path: '%s'.", bake_config.bakefile_path)
            os.exit(1)
        }
    }
    printfln_verbose("Bakefile Path: '%s'", bake_config.bakefile_path)
    bakefile_bytes, err := os.read_entire_file(bake_config.bakefile_path, context.temp_allocator)
    if err != nil {
        printfln_error("Failed to open bakefile: '%s'\n", bake_config.bakefile_path)
        os.exit(1)
    }
    init_builtins()
    parser := parser_make(transmute(string) bakefile_bytes)
    stmts := parse_stmts(&parser)
    printfln_verbose("Running the bake script...")
    ctx := exec_stmts(transmute(string) bakefile_bytes, stmts)
    cli_args := make([dynamic]Value)
    for arg in extra_args[:] {
        append(&cli_args, arg)
    }
    mb_value := env_get(ctx.global_env, bake_config.bakefile_target)
    printfln_verbose("Finding the target function '%s'...", bake_config.bakefile_target)
    if value, ok := mb_value.?; !ok {
        printfln_error("Target function '%s' not declared in the script.", bake_config.bakefile_target)
        printfln_color("* Please declare function as `func 'cli' %s () {}`", bake_config.bakefile_target)
        os.exit(1)
    } else if func, ok := value.(Stmt_Func); !ok {
        printfln_error("Target '%s' is not a function.", bake_config.bakefile_target)
        printfln_color("* Please declare function as `func 'cli' %s () {}`", bake_config.bakefile_target)
        os.exit(1)
    } else {
        printfln_verbose("Executing the target function '%s'...", bake_config.bakefile_target)
        ret, err := call_func(&ctx, ctx.global_env, value.(Stmt_Func), []Value{cli_args[:]})
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
