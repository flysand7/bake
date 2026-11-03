package bake_cache

import "core:mem"

/*
    Magic number for the `.bake-cache` file.

    This corresponds to ASCII string "bake".
*/
FORMAT_MAGIC :: u32(0x62616B65)

/*
    Latest format version of the `.bake-cache` file.
*/
FORMAT_VERSION :: u32(1)

/*
    Type of errors that may be returned by the cache format parser.
*/
Format_Error_Code :: enum {
    None,
    Invalid_File_Size,
    Invalid_Offset,
    Missing_Magic,
    Outdated_Version,
    Unknown_Version,
}

/*
    An error that may be returned by the cache format parser.
*/
Format_Error :: struct {
    code: Format_Error_Code,
    offset: uintptr,
    value: u64,
}

/*
    Creates a format error struct.
*/
make_format_error :: proc(code: Format_Error_Code, offset: uintptr, value := u64(0)) -> Format_Error {
    return {
        code = code,
        offset = offset,
        value = value,
    }
}

/*
    The header of the `.bake-cache` file.
*/
Format_Header :: struct {
    // Magic number. Must be the string "bake" in ASCII.
    magic: u32,
    // The format version. Must be exactly the version specified in the FORMAT_VERSION.
    version: u32,
    // Number of file entries stored in the cache.
    file_entry_count: u32,
    // Offset of the string table, where the filenames are stored.
    string_table_offset: u32,
    // Size of the string table.
    string_table_size: u32,
}

/*
    The entry corresponding to a file.
*/
Format_File_Entry :: struct {
    // The offset of the file path string in the string table.
    path_offset: u32,
    // The last known modification time of the file.
    mtime: u64,
    // The last known size of the file.
    size: u64,
    // The last known hash of the file
    content_hash: u128,
}

/*
    Parses the contents of the `.bake-cache` file.
*/
parse_format :: proc(cache_file_path: string, bytes: []byte) -> (Cache, Format_Error) {
    if len(bytes) < size_of(Format_Header) {
        return {}, make_format_error(.Invalid_File_Size, 0, size_of(Format_Header))
    }
    header := cast(^Format_Header) raw_data(bytes)
    if header.magic != FORMAT_MAGIC {
        return {}, make_format_error(.Missing_Magic, offset_of(header.magic), u64(header.magic))
    }
    if header.version < FORMAT_VERSION {
        return {}, make_format_error(.Outdated_Version, offset_of(header.version), u64(header.version))
    }
    if header.version > FORMAT_VERSION {
        return {}, make_format_error(.Unknown_Version, offset_of(header.version), u64(header.version))
    }
    expected_file_size := int(
        size_of(Format_Header) +
        size_of(Format_File_Entry)*header.file_entry_count +
        header.string_table_size
    )
    if len(bytes) != expected_file_size {
        return {}, make_format_error(.Invalid_File_Size, 0, u64(expected_file_size))
    }
    if int(header.string_table_offset) >= len(bytes) {
        return {}, make_format_error(.Invalid_Offset, offset_of(header.string_table_offset), u64(header.string_table_offset))
    }
    string_table_offs := header.string_table_offset
    string_table_sz := header.string_table_size
    string_table := bytes[string_table_offs:string_table_offs+string_table_sz]
    file_entry_offs := size_of(Format_Header)
    file_entry_table := (cast([^]Format_File_Entry) &bytes[file_entry_offs])[:header.file_entry_count]
    fs_cache := new(Fs_Cache)
    fs_cache.invalidated_files = make([dynamic]string, context.allocator)
    fs_cache.recs = make([dynamic]Fs_Cache_Rec, context.allocator)
    fs_cache.path = cache_file_path
    for file_entry, file_entry_idx in file_entry_table {
        path_offset := file_entry.path_offset
        if path_offset >= string_table_sz {
            field_offset := size_of(Format_Header) + 
                file_entry_idx * size_of(Format_File_Entry) +
                int(offset_of(file_entry.path_offset))
            return {}, make_format_error(.Invalid_Offset, uintptr(field_offset), u64(path_offset))
        }
        file_path := string(cstring(&string_table[path_offset]))
        append(&fs_cache.recs, Fs_Cache_Rec {
            path = file_path,
            mtime = file_entry.mtime,
            size = file_entry.size,
            content_hash = file_entry.content_hash,
        })
    }
    return _fs_cache_impl(fs_cache), {}
}

write_format :: proc(cache: ^Fs_Cache, a := context.allocator) -> []byte {
    bytes := make([dynamic]byte, a)
    non_zero_resize(&bytes, size_of(Format_Header))
    file_table_offs := len(bytes)
    non_zero_resize(&bytes, len(bytes) + len(cache.recs) * size_of(Format_File_Entry))
    string_table_offs := len(bytes)
    header := transmute(^Format_Header) &bytes[0]
    header.magic = FORMAT_MAGIC
    header.version = FORMAT_VERSION
    header.file_entry_count = u32(len(cache.recs))
    header.string_table_offset = u32(string_table_offs)
    file_table := transmute([^]Format_File_Entry) &bytes[file_table_offs]
    non_zero_resize(&bytes, len(bytes) + 1)
    bytes[string_table_offs] = 0
    string_table := transmute([^]string) &bytes[string_table_offs]
    string_table_idx := 0
    for rec, i in cache.recs {
        path_offset := len(bytes)
        non_zero_resize(&bytes, len(bytes) + len(rec.path)+1)
        mem.copy(&bytes[path_offset], raw_data(rec.path), len(rec.path))
        bytes[path_offset + len(rec.path)] = 0
        file_table[i].path_offset = u32(path_offset - string_table_offs)
        file_table[i].mtime = rec.mtime
        file_table[i].size = rec.size
        file_table[i].content_hash = rec.content_hash
    }
    header.string_table_size = u32(len(bytes)) - header.string_table_offset
    return bytes[:]
}
