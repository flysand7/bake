package bake

import "core:os"
import "core:fmt"

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
        printfln_verbose("RUN COMMAND: %v", cmd)
        return run_system_cmd(cmd)
    }
    unreachable()
}

run_system_cmd :: proc(argv: []string) -> (int, Exec_Error) {
    state, stdout, stderr, err := os.process_exec(os.Process_Desc {
        command = argv,
    }, context.allocator)
    if err != nil {
        if err == .Not_Exist {
            return 127, nil
        } else {
            fmt.panicf("Unable to start process: %v", err)
        }
    }
    return state.exit_code, nil
}
