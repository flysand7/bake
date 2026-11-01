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

/* Automatically highlights tokens in a string */
auto_color :: proc(s: string) -> string {
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
            strings.write_string(&sb, COLOR_RESET)
        }
        switch s[i] {
        case '-':
            if !arg && spc {
                arg = true
                strings.write_string(&sb, COLOR_ARG)
                strings.write_byte(&sb, '-')
            } else {
                strings.write_byte(&sb, '-')
            }
        case '`':
            if !m {
                strings.write_byte(&sb, '`')
                strings.write_string(&sb, COLOR_CODE)
            } else {
                strings.write_string(&sb, COLOR_RESET)
                strings.write_byte(&sb, '`')
            }
            m = !m
        case '\'':
            if !m {
                strings.write_byte(&sb, '\'')
                strings.write_string(&sb, COLOR_USER)
            } else {
                strings.write_string(&sb, COLOR_RESET)
                strings.write_byte(&sb, '\'')
            }
            m = !m
        case '[':
            strings.write_byte(&sb, '[')
            strings.write_string(&sb, COLOR_ARG)
        case ']':
            strings.write_string(&sb, COLOR_RESET)
            strings.write_byte(&sb, ']')
        case '<':
            strings.write_byte(&sb, '<')
            strings.write_string(&sb, COLOR_PAR)
        case '>':
            strings.write_string(&sb, COLOR_RESET)
            strings.write_byte(&sb, '>')
        case:
            strings.write_byte(&sb, s[i])
        }
    }
    return strings.to_string(sb)
}
