package bake_cli

when ODIN_OS == .Windows {
	foreign import libc "system:libucrt.lib"
} else when ODIN_OS == .Darwin {
	foreign import libc "system:System"
} else {
	foreign import libc "system:c"
}

@(default_calling_convention="c")
foreign libc {
    _isatty :: proc(fd: i32) -> b32 ---
}

import "core:os"

/*
    Checks whether the file descriptor refers to a tty

    This returns `true` if the file descriptor refers to a terminal, and `false`
    if it doesn't, e.g. when this program's output has been redirected to a
    file.
*/
isatty :: #force_inline proc(f: ^os.File) -> bool {
    fd: i32 = ---
    switch f {
    case os.stdin: fd = 0
    case os.stdout: fd = 1
    case os.stderr: fd = 2
    case:
        // Note: I'm too dumb to implement this properly.
        panic("This function expects only std file handles")
    }
    return bool(_isatty(fd))
}
