package bake

import "cli"
import "core:os"
import "core:log"
import "core:fmt"
import "core:strings"

@(private="file")
bake_logger_proc :: proc(
    data: rawptr,
    level: log.Level,
    text: string,
    options: log.Options,
    location := #caller_location,
) {
    sb := strings.builder_make()
    output: ^os.File = ---
    is_output_colored: bool
    if level < .Error {
        output = os.stdout
        is_output_colored = cli.config.stdout_color
    } else {
        output = os.stderr
        is_output_colored = cli.config.stderr_color
    }
    level_str: string = ---
    switch level {
    case .Debug:   level_str = "[VERBOSE] "
    case .Info:    level_str = ""
    case .Warning: level_str = "[WARNING] "
    case .Error:   level_str = "[ERROR] "
    case .Fatal:   level_str = "[FATAL] "
    }
    line_color: string = ---
    switch level {
    case .Debug:   line_color = cli.COLOR_VERBOSE
    case .Info:    line_color = ""
    case .Warning: line_color = cli.COLOR_ERROR
    case .Error:   line_color = cli.COLOR_ERROR
    case .Fatal:   line_color = cli.COLOR_ERROR
    }
    text := text
    if level == .Info && is_output_colored {
        text = cli.auto_color(text)
    }
    if is_output_colored {
        strings.write_string(&sb, line_color)
    }
    strings.write_string(&sb, level_str)
    strings.write_string(&sb, text)
    if is_output_colored {
        strings.write_string(&sb, cli.COLOR_RESET)
    }
    fmt.printfln("%s", strings.to_string(sb))
}

bake_logger :: proc(level: log.Level, options: log.Options) -> log.Logger {
    return {
        data = nil,
        lowest_level = level,
        options = options,
        procedure = bake_logger_proc,
    }
}
