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
        // TODO(flysand): Support string cmdline arguments to run_cmd.
        panic("Running via cmdline is temporarily not supported")
    case []string:
        return run_system_cmd(cmd)
    }
    unreachable()
}

run_system_cmd :: proc(argv: []string) -> (int, Exec_Error) {
    state, stdout, stderr, err := os.process_exec(os.Process_Desc {
        command = argv,
    }, context.allocator)
    if err != nil {
        panic("Unable to start process")
    }
    return state.exit_code, nil
}
