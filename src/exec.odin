package bake

import "core:os"

Exec_Error :: enum {
    None,
    Unhandled_Error,
    Executable_Not_Found,
}

run_cmd :: proc(command: Cmd) -> (int, Exec_Error) {
    switch cmd in command {
        case Builtin_Cmd:
            return cmd.execute(cmd.args), .None
        case string:
            return run_shell_cmd(cmd)
        case []string:
            return run_system_cmd(cmd)
    }
    unreachable()
}

run_shell_cmd :: proc(command_line: string) -> (int, Exec_Error) {
    return _os_exec_cmdline(command_line)
}

run_system_cmd :: proc(argv: []string) -> (int, Exec_Error) {
    return _os_exec_argv(argv)
}
