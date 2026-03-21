//+build windows
package bake

import "core:sys/windows"
import "core:os"

File_Id :: struct {
    file_index: u64,
    vol_serial: u32,
}

file_get_id :: proc(name: string) -> (File_Id, u64, bool) {
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
        return {}, 0, false
    }
    defer windows.CloseHandle(file_handle)
    info := windows.BY_HANDLE_FILE_INFORMATION{}
    if !windows.GetFileInformationByHandle(file_handle, &info) {
        return {}, 0, false
    }
	last_write_time := u64(info.ftLastWriteTime.dwLowDateTime) | u64(info.ftLastWriteTime.dwHighDateTime) << 32
    file_id := File_Id {
        file_index = u64(info.nFileIndexLow) | u64(info.nFileIndexHigh)<<32,
        vol_serial = info.dwVolumeSerialNumber,
    }
    return file_id, last_write_time, true
}
