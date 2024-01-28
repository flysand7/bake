package bake

import "core:os"
import "core:fmt"

loc_extract_line_col :: proc(pos: int, text: string) -> (int, int) {
    newline_count := 0
    newline_offs := -1
    for offs in 0 ..< len(text) {
        if offs >= pos {
            break
        }
        if text[offs] == '\n' {
            newline_count += 1
            newline_offs = offs
        }
    }
    line := newline_count + 1
    offs := pos - newline_offs
    return line, offs
}

lexing_error :: proc(p: ^Parser, msg: string) -> ! {
    line, col := loc_extract_line_col(p.idx-1, p.text)
    fmt.eprintf("Syntax Error(%d, %d): %s\n", line, col, msg)
    os.exit(1)
}

parse_error :: proc(p: ^Parser, loc: Loc, msg: string) -> ! {
    line, col := loc_extract_line_col(loc.offs, p.text)
    fmt.eprintf("Syntax Error(%d, %d): %s\n", line, col, msg)
    os.exit(1)
}

script_error :: proc(c: ^Ctx, loc: Loc, msg: string) -> ! {
    line, col := loc_extract_line_col(loc.offs, c.text)
    fmt.eprintf("Error(%d, %d): %s\n", line, col, msg)
    os.exit(1)
}

lexing_errorf :: proc(p: ^Parser, format: string, args: ..any) -> ! {
    line, col := loc_extract_line_col(p.idx-1, p.text)
    fmt.eprintf("Syntax Error(%d, %d): %s\n", line, col, fmt.tprintf(format, ..args))
    os.exit(1)
}

parse_errorf :: proc(p: ^Parser, loc: Loc, format: string, args: ..any) -> !{
    line, col := loc_extract_line_col(loc.offs, p.text)
    fmt.eprintf("Syntax Error(%d, %d): %s\n", line, col, fmt.tprintf(format, ..args))
    os.exit(1)
}

script_errorf :: proc(c: ^Ctx, loc: Loc, format: string, args: ..any) -> ! {
    line, col := loc_extract_line_col(loc.offs, c.text)
    fmt.eprintf("Error(%d, %d): %s\n", line, col, fmt.tprintf(format, ..args))
    os.exit(1)
}
