//+build windows
package bake

import "core:sys/windows"
import "core:os"

File :: struct {
    write_time: u64,
    file_index: u64,
    vol_serial: u32,
}

file_is_same :: proc(f1, f2: File) -> bool {
    return f1.file_index == f2.file_index && f1.vol_serial == f2.vol_serial
}

file_make :: proc(name: string) -> (File, bool) {
    wname := windows.utf8_to_wstring(name)
    file_handle := windows.CreateFileW(
        wname,
        windows.GENERIC_READ,
        windows.FILE_SHARE_DELETE|windows.FILE_SHARE_READ|windows.FILE_SHARE_WRITE,
        nil,
        windows.OPEN_EXISTING,
        windows.FILE_ATTRIBUTE_NORMAL,
        nil,
    )
    if file_handle == windows.INVALID_HANDLE_VALUE {
        return {}, false
    }
    defer windows.CloseHandle(file_handle)
    info := windows.BY_HANDLE_FILE_INFORMATION{}
    if !windows.GetFileInformationByHandle(file_handle, &info) {
        return {}, false
    }
	last_write_time := u64(info.ftLastWriteTime.dwLowDateTime) | u64(info.ftLastWriteTime.dwHighDateTime) << 32
    volume_serial := info.dwVolumeSerialNumber
    file_index := u64(info.nFileIndexLow)|u64(info.nFileIndexHigh)<<32
    return File {
        write_time = last_write_time,
        file_index = file_index,
        vol_serial = volume_serial,
    }, true
}

file_make1 :: proc(name: string) -> File {
    file, ok := file_make(name)
    if !ok {
        panic("Unable to create file")
    }
    return file
}
