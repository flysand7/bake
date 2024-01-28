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
    for arg in os.args[1:] {
        if arg == "-h" || arg == "--help" || arg == "-help" {
            fmt.println(HELP_MESSAGE)
            os.exit(2)
        } else if arg == "-verbose" {
            verbose = true
        } else if strings.has_prefix(arg, "-bakefile:") {
            bakefile_path = arg[len("-bakefile:"):]
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
    exec_stmts(stmts)
}
