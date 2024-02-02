package bake

import "core:strings"
import "core:fmt"
import "core:os"

HELP_MESSAGE :: `bake - Some kind of build system

Syntax:
  bake [target] [options..]

Available options:
  -verbose         Print verbose logs.
  -bakefile:<path> Explicitly specify path to bakefile.
`

verbose := false

printf_verbose :: proc(fmt_string: string, args: ..any) -> int {
    if verbose {
        return fmt.printf(fmt_string, ..args)
    } else {
        return 0
    }
}

main :: proc() {
    verbose := false
    bakefile_path := ""
    extra_args_idx := Maybe(int) {}
    for arg, arg_idx in os.args[1:] {
        if arg == "-h" || arg == "--help" || arg == "-help" {
            fmt.println(HELP_MESSAGE)
            os.exit(2)
        } else if arg == "-verbose" {
            verbose = true
        } else if arg == "--" {
            extra_args_idx = arg_idx+1
        } else if strings.has_prefix(arg, "-bakefile:") {
            bakefile_path = arg[len("-bakefile:"):]
        }
    }
    extra_args := make([dynamic]string)
    if start_idx, ok := extra_args_idx.?; ok {
        for arg, arg_idx in os.args[start_idx+1:] {
            append(&extra_args, arg)
        }
    }
    if len(bakefile_path) == 0 {
        bakefile_path = find_bakefile()
        if len(bakefile_path) == 0 {
            fmt.eprintf("Error finding the bakefile.")
            os.exit(1)
        }
    }
    printf_verbose("Bakefile Path: %s\n", bakefile_path)
    bakefile_bytes, ok := os.read_entire_file(bakefile_path)
    if !ok {
        fmt.eprintf("Failed to open bakefile: %s\n", bakefile_path)
        os.exit(1)
    }
    parser := parser_make(transmute(string) bakefile_bytes)
    stmts := parse_stmts(&parser)
    ctx := exec_stmts(transmute(string) bakefile_bytes, stmts)
    if extra_args_idx != nil {
        cli_func_name := extra_args[0]
        cli_args := make([dynamic]Value)
        for arg in extra_args[1:] {
            append(&cli_args, arg)
        }
        mb_value := env_get(ctx.global_env, cli_func_name)
        if value, ok := mb_value.?; !ok {
            panic("Not exist")
        } else if func, ok := value.(Stmt_Func); !ok {
            panic("Not a function")
        } else {
            ret, err := call_func(&ctx, ctx.global_env, value.(Stmt_Func), []Value{cli_args[:]})
            if tok, ok := err.(Call_Func_Err_CF_Token); ok && tok.tok != nil {
                panic("Cotrol flow statement used outside of while loop")
            } else if _, ok := err.(Call_Func_Err_Param_Mismatch); ok {
                panic("Param mismatch")
            }
        }
    }
}
