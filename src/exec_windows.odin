//+build windows
package bake

import "core:sys/windows"
import "core:strings"
import "core:slice"
import "core:mem"

_os_exec_argv :: proc(argv: []string) -> (int, Exec_Error) {
    assert(len(argv) > 0)
    app_name := windows.utf8_to_wstring(argv[0])
    cmd_builder := make([dynamic]u16)
    for arg, arg_idx in argv[:] {
        if arg_idx != 0 {
            append(&cmd_builder, ' ')
        }
        qarg := _win_escape_string(arg)
        warg := windows.utf8_to_wstring(qarg)
        for i := 0; warg[i] != 0; i += 1 {
            append(&cmd_builder, warg[i])

        }
    }
    append(&cmd_builder, 0)
    #no_bounds_check cmd_line := &cmd_builder[0]
    return _win_exec_cmdline(app_name, cmd_line)
}

_os_exec_cmdline :: proc(cmd_line: string) -> (int, Exec_Error) {
    wcmd_line := windows.utf8_to_wstring(cmd_line)
    return _win_exec_cmdline(nil, wcmd_line)
}

@(private="file")
_win_exec_cmdline :: proc(app_name: windows.wstring, cmd_line: windows.wstring) -> (int, Exec_Error) {
    app_name := app_name
    if app_name != nil {
        if found := find_executable_in_cwd(app_name); found != nil {
            app_name = found
        } else if found := find_executable_in_env(app_name); found != nil {
            app_name = found
        } else {
            return -1, .Executable_Not_Found
        }
    }
    process_info := windows.PROCESS_INFORMATION {}
    process_ok := windows.CreateProcessW(
        app_name,
        cmd_line,
        nil,
        nil,
        false, // inherit handles
        0, // flags
        nil,
        nil,
        &windows.STARTUPINFOW {},
        &process_info,
    )
    if !process_ok {
        win_err := windows.GetLastError()
        printf_verbose("CreateProcess Error: %v\n", win_err)
        if win_err == windows.ERROR_FILE_NOT_FOUND {
            return -1, .Executable_Not_Found
        }
        return -1, .Unhandled_Error
    }
    defer windows.CloseHandle(process_info.hProcess)
    defer windows.CloseHandle(process_info.hThread)
    wait_status := windows.WaitForSingleObject(process_info.hProcess, windows.INFINITE)
    if wait_status == windows.WAIT_FAILED {
        printf_verbose("WaitForSingleObject Error: %v\n", windows.GetLastError())
        return -1, .Unhandled_Error
    }
    exit_code := windows.DWORD(0)
    exit_code_ok := windows.GetExitCodeProcess(
        process_info.hProcess,
        &exit_code,
    )
    if !exit_code_ok {
        printf_verbose("GetExitCode Crror: %v\n", windows.GetLastError())
        return -1, .Unhandled_Error
    }
    return cast(int) exit_code, .None
}

@(private="file")
_win_escape_string :: proc(str: string) -> string {
    builder := strings.builder_make()
    for i in 0 ..< len(str) {
        ch := str[i]
        if ch == '"' || ch == '\\' {
            strings.write_byte(&builder, '\\')
        }
        strings.write_byte(&builder, ch)
    }
    return strings.to_string(builder)
}

find_executable_in_cwd :: proc(exe_name: windows.wstring) -> windows.wstring {
    cwd_buf_len := windows.GetCurrentDirectoryW(0, nil)
    cwd_buf := make([]windows.WCHAR, cwd_buf_len)
    wr_len := windows.GetCurrentDirectoryW(cwd_buf_len, raw_data(cwd_buf))
    cwd_buf = cwd_buf[:wr_len]
    if full_name, ok := dir_contains(cwd_buf, exe_name); ok {
        return full_name
    }
    return nil
}

find_executable_in_env :: proc(exe_name: windows.wstring) -> windows.wstring {
    path_buf_len := windows.GetEnvironmentVariableW(windows.L("PATH"), nil, 0)
    path_buf := make([]windows.WCHAR, path_buf_len)
    assert(path_buf_len != 0)
    wr_len := windows.GetEnvironmentVariableW(windows.L("PATH"), &path_buf[0], path_buf_len)
    assert(wr_len+1 == path_buf_len)
    path_buf = path_buf[:wr_len]
    idx := 0
    for idx < len(path_buf) {
        start := idx
        for idx < len(path_buf) && path_buf[idx] != ';' {
            idx += 1
        }
        end := idx
        idx += 1
        path := path_buf[start:end]
        if full_name, ok := dir_contains(path, exe_name); ok {
            return full_name
        }
    }
    return nil
}

dir_contains :: proc(path: []windows.WCHAR, name: windows.wstring) -> (windows.wstring, bool) {
    name_len := 0
    for i := 0; name[i] != 0; i += 1 {
        name_len += 1
    }
    full_name := make([]windows.WCHAR, len(path) + name_len + 2)
    mem.copy(raw_data(full_name), raw_data(path), len(path) * size_of(windows.WCHAR))
    full_name[len(path)] = '\\'
    mem.copy(raw_data(full_name[1+len(path):]), name, name_len * size_of(windows.WCHAR))
    full_name[len(full_name)-1] = 0
    attrib := windows.GetFileAttributesW(raw_data(full_name))
    exists := (attrib != windows.INVALID_FILE_ATTRIBUTES) && ((attrib & windows.FILE_ATTRIBUTE_DIRECTORY) == 0)
    return raw_data(full_name), exists
}
