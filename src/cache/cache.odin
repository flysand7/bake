package bake_cache

import "core:os"
import "core:log"
import "core:hash"

/* Status of a file inside a cache */
Cache_Status :: enum {
    // The file is unchanged since last build.
    Unchanged,
    // The requested file doesn't exist in a filesystem, even if it is recorded
    // inside the cache.
    Not_Exist,
    // The file is recorded inside the cache, but was updated in a filesystem.
    Updated,
    // The file was not changed nor deleted, however during planning the
    // dependencies of this file were unvalidated.
    Invalidated,
}

/* The cache interface */
Cache :: struct {
    // Context to store cached data.
    ctx: rawptr,
    // The `cache_update` procedure implementation.
    _impl_update: proc(ctx: rawptr, filename: string),
    // The `cache_invlaidate` procedure implementation.
    _impl_invalidate: proc(ctx: rawptr, filename: string),
    // The `cache_check` procedure implementation.
    _impl_check: proc(ctx: rawptr, filename: string) -> Cache_Status,
}

@(private)
cache_log :: proc(ic: ^Cache, format: string, args: ..any) {
    log.debugf(format, ..args)
}

/*
Cache a file or update its state inside the cache.

This should record the requested file's information inside the cache.
Information, that is necessary to tell whether or not file was changed, at some
later point will be recorded inside `ic`. If the file is new, a new cache
record will be created. If the file already exists inside the cache at the point
of calling this function, the cache record will be overwritten with the new
information.
*/
cache_update :: proc(ic: ^Cache, filename: string) {
    cache_log(ic, "* Cached file: '%s'", filename)
    ic._impl_update(ic.ctx, filename)
}

/*
Invalidate a file in cache.

This changes the file record's status to `.Invalidated`, which causes all
subsequent cache checks to return this status and treat this file the same way
as updated files.
*/
cache_invalidate :: proc(ic: ^Cache, filename: string) {
    cache_log(ic, "* Invalidated file: '%s'", filename)
    ic._impl_invalidate(ic.ctx, filename)
}

/*
Check file state.

This should return information about whether the file is recorded inside the
cache and whether it was updated or not.
*/
cache_check :: proc(ic: ^Cache, filename: string) -> Cache_Status {
    status := ic._impl_check(ic.ctx, filename)
    cache_log(ic, "* File '%s' status: %v", filename, status)
    return status
}

Fs_Cache_Rec :: struct {
    path: string,
    mtime: u64,
    size: u64,
    content_hash: u128,
}

Fs_Cache :: struct {
    path: string,
    recs: [dynamic]Fs_Cache_Rec,
    invalidated_files: [dynamic]string
}

fs_cache_open :: proc(path: string) -> Cache {
    bytes, file_err := os.read_entire_file(path, context.allocator)
    // If the cache doesn't exist, a new one should be created.
    if file_err == .Not_Exist {
        fs_cache := new(Fs_Cache)
        fs_cache.path = path
        fs_cache.recs = make([dynamic]Fs_Cache_Rec, context.allocator)
        fs_cache.invalidated_files = make([dynamic]string, context.allocator)
        return _fs_cache_impl(fs_cache)
    }
    if file_err != nil {
        log.panicf("Error reading the cache file '%s' (%v)", path, file_err)
    }
    cache, cache_err := parse_format(path, bytes)
    need_regenerate_cache := false
    switch cache_err.code {
    case .None:
        /* nothing */
    case .Invalid_File_Size:
        log.panicf("Invalid cache file: Invalid file size: %d bytes (%d bytes expected)", len(bytes), cache_err.value)
    case .Invalid_Offset:
        log.panicf("Invalid cache file: Offset at (%08x) is out of bounds (%08x)", cache_err.offset, cache_err.value)
    case .Missing_Magic:
        log.warnf("Invalid cache file: No magic number at the start of the file")
        need_regenerate_cache = true
    case .Outdated_Version:
        log.warnf("Invalid cache file: Format version is outdated (%d)", cache_err.value)
        need_regenerate_cache = true
    case .Unknown_Version:
        log.warnf("Invalid cache file: Unknown format version (%d)", cache_err.value)
        need_regenerate_cache = true
    }
    if need_regenerate_cache {
        log.warnf("Cache file will be regenerated due to errors")
        fs_cache := new(Fs_Cache)
        fs_cache.path = path
        fs_cache.recs = make([dynamic]Fs_Cache_Rec, context.allocator)
        fs_cache.invalidated_files = make([dynamic]string, context.allocator)
        return _fs_cache_impl(fs_cache)
    }
    return cache
}

fs_cache_make :: proc() -> Cache {
    cache := new(Fs_Cache)
    cache.recs = make([dynamic]Fs_Cache_Rec)
    return _fs_cache_impl(cache)
}

fs_cache_update :: proc(cache: rawptr, filename: string) {
    cache := cast(^Fs_Cache) cache
    file_info, error := os.stat(filename, context.allocator)
    if error != nil {
        log.panicf("Cache: Unable to stat file: '%s' (%v)", filename, error)
    }
    file_rec: ^Fs_Cache_Rec = nil
    for &rec in cache.recs {
        if rec.path == filename {
            file_rec = &rec
            break
        }
    }
    contents, read_error := os.read_entire_file(filename, context.allocator)
    if read_error != nil {
        log.panicf("Cache: Unable to open file: '%s' (%v)", filename, read_error)
    }
    content_hash := hash.murmur3_x64_128(contents)
    if file_rec != nil {
        file_rec.mtime = u64(file_info.modification_time._nsec)
        file_rec.size = u64(file_info.size)
        file_rec.content_hash = content_hash
    } else {
        append(&cache.recs, Fs_Cache_Rec {
            mtime = u64(file_info.modification_time._nsec),
            size = u64(file_info.size),
            path = filename,
            content_hash = content_hash
        })
    }
}

fs_cache_invalidate :: proc(cache: rawptr, filename: string) {
    cache := cast(^Fs_Cache) cache
    append(&cache.invalidated_files, filename)
}

fs_cache_check :: proc(cache: rawptr, filename: string) -> Cache_Status {
    cache := cast(^Fs_Cache) cache
    file_info, file_info_err := os.stat(filename, context.allocator)
    if file_info_err != nil {
        if file_info_err == .Not_Exist {
            return .Not_Exist
        }
        log.panicf("Unable to stat file: '%s' (%v)", filename, file_info_err)
    }
    for invalidated_path in cache.invalidated_files {
        if filename == invalidated_path {
            return .Invalidated
        }
    }
    rec: ^Fs_Cache_Rec = nil
    for &r in cache.recs {
        if r.path == filename {
            rec = &r
            break
        }
    }
    if rec == nil {
        return .Updated
    }
    file_mtime := u64(file_info.modification_time._nsec)
    file_size := u64(file_info.size)
    if file_mtime != rec.mtime || file_size != rec.size {
        return .Updated
    }
    return .Unchanged
}

fs_cache_write :: proc(ic: ^Cache) {
    cache := cast(^Fs_Cache) ic.ctx
    bytes := write_format(cache, context.allocator)
    perm := os.Permissions {
        .Read_Other, .Read_Group, .Read_User, .Write_User
    }
    err := os.write_entire_file(cache.path, bytes, perm)
    if err != nil {
        log.panicf("Error writing to cache '%s' (%v)", cache.path, err)
    }
    cache_log(ic, "CACHE WRITTEN: '%s'", cache.path)
}

_fs_cache_impl :: proc(fs_cache: ^Fs_Cache) -> Cache {
    return Cache {
        ctx = fs_cache,
        _impl_update = fs_cache_update,
        _impl_invalidate = fs_cache_invalidate,
        _impl_check = fs_cache_check,
    }
}

Test_Cache_Rec :: struct {
    filename: string,
    status: Cache_Status,
}

Test_Cache :: struct {
    recs: []Test_Cache_Rec,
    invalidated_files: [dynamic]string,
}

test_cache_make :: proc(recs: []Test_Cache_Rec) -> Cache {
    cache := new(Test_Cache)
    cache.recs = recs
    cache.invalidated_files = make([dynamic]string)
    return Cache {
        ctx = cache,
        _impl_update = test_cache_update,
        _impl_invalidate = test_cache_invalidate,
        _impl_check = test_cache_check,
    }
}

test_cache_update :: proc(cache: rawptr, filename: string) {
    panic("test_cache_update can't cache new files.")
}

test_cache_invalidate :: proc(cache: rawptr, filename: string) {
    cache := cast(^Test_Cache) cache
    append(&cache.invalidated_files, filename)
}

test_cache_check :: proc(cache: rawptr, filename: string) -> Cache_Status {
    cache := cast(^Test_Cache) cache
    for i in cache.invalidated_files {
        if i == filename {
            return .Invalidated
        }
    }
    for rec in cache.recs {
        if rec.filename == filename {
            return rec.status
        }
    }
    return .Not_Exist
}

