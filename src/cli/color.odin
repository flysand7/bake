package bake_cli

import "core:strings"

COLOR_RESET :: "\e[0m"
COLOR_VERBOSE :: "\e[38;5;234m"
COLOR_ERROR :: "\e[38;5;197m"
COLOR_USER :: "\e[38;5;121m"
COLOR_CODE :: "\e[38;5;104m"
COLOR_ARG :: "\e[38;5;220m"
COLOR_PAR :: "\e[38;5;185m"

/*
    Removes coloring tokens from the string.
*/
strip_color :: proc(s: string, a := context.allocator) -> string {
    sb := strings.builder_make(a)
    i := 0
    c := false
    for i in 0 ..< len(s) {
        if s[i] == '\e' {
            c = true
        } else if c && s[i] == 'm' {
            c = false
        } else if !c {
            strings.write_byte(&sb, s[i])
        }
    }
    return strings.to_string(sb)
}
