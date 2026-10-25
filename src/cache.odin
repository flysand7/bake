package bake

import "core:slice"
import "core:os"

/* Status of a file inside a cache */
Cache_Status :: enum {
    // The file is unchanged since last build.
    Unchanged,
    // The requested file doesn't exist in a filesystem, even if it is recorded
    // inside the cache.
    Not_Exist,
    // The file is recorded inside the cache, but was updated in a filesystem.
    Updated,
}

/* The cache interface */
Cache :: struct {
    // Context to store cached data.
    ctx: rawptr,
    // The `cache_update` procedure implementation.
    _impl_update: proc(ctx: rawptr, filename: string),
    // The `cache_check` procedure implementation.
    _impl_check: proc(ctx: rawptr, filename: string) -> Cache_Status,
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
    printfln_verbose("* Cached file: '%s'", filename)
    ic._impl_update(ic.ctx, filename)
}

/*
Check file state.

This should return information about whether the file is recorded inside the
cache and whether it was updated or not.
*/
cache_check :: proc(ic: ^Cache, filename: string) -> Cache_Status {
    status := ic._impl_check(ic.ctx, filename)
    printfln_verbose("* File '%s' status: %v", filename, status)
    return status
}

Fs_Cache_Rec :: struct {
    id: File_Id,
    ts: u64,
}

Fs_Cache :: struct {
    recs: [dynamic]Fs_Cache_Rec,
}

fs_cache_open :: proc() -> Cache {
    bytes, err := os.read_entire_file(bake_config.bake_cache_path, context.allocator)
    cache := new(Fs_Cache)
    cache.recs = make([dynamic]Fs_Cache_Rec)
    if err != nil {
        // If the cache file doesn't exist, this behaves like cache_make.
        return _fs_cache_impl(cache)
    }
    prev_recs := slice.reinterpret([]Fs_Cache_Rec, bytes)
    append(&cache.recs, ..prev_recs)
    return _fs_cache_impl(cache)
}

fs_cache_make :: proc() -> Cache {
    cache := new(Fs_Cache)
    cache.recs = make([dynamic]Fs_Cache_Rec)
    return _fs_cache_impl(cache)
}

fs_cache_update :: proc(cache: rawptr, filename: string) {
    cache := cast(^Fs_Cache) cache
    file_id, file_ts, ok := file_get_id(filename)
    for &rec in cache.recs {
        if rec.id == file_id {
            rec.ts = file_ts
            return
        }
    }
    append(&cache.recs, Fs_Cache_Rec { file_id, file_ts })
}

fs_cache_check :: proc(cache: rawptr, filename: string) -> Cache_Status {
    cache := cast(^Fs_Cache) cache
    rec: Fs_Cache_Rec = ---
    rec_found := false
    file_id, file_ts, ok := file_get_id(filename)
    if !ok {
        return .Not_Exist
    }
    for r in cache.recs {
        if r.id == file_id {
            rec = r
            rec_found = true
            break
        }
    }
    if file_ts > rec.ts {
        return .Updated
    }
    return .Unchanged
}

fs_cache_write :: proc(cache: ^Cache) {
    cache := cast(^Fs_Cache) cache.ctx
    bytes := slice.reinterpret([]u8, cache.recs[:])
    perm := os.Permissions {
        .Read_Other, .Read_Group, .Read_User, .Write_User
    }
    err := os.write_entire_file(bake_config.bake_cache_path, bytes, perm)
    assert(err == nil)
    printfln_verbose("CACHE WRITTEN: '%s'", bake_config.bake_cache_path)
}

_fs_cache_impl :: proc(fs_cache: ^Fs_Cache) -> Cache {
    return Cache {
        ctx = fs_cache,
        _impl_update = fs_cache_update,
        _impl_check = fs_cache_check,
    }
}

Test_Cache_Rec :: struct {
    filename: string,
    status: Cache_Status,
}

Test_Cache :: struct {
    recs: []Test_Cache_Rec,
}

test_cache_make :: proc(recs: []Test_Cache_Rec) -> Cache {
    cache := new(Test_Cache)
    cache.recs = recs
    return Cache {
        ctx = cache,
        _impl_update = test_cache_update,
        _impl_check = test_cache_check,
    }
}

test_cache_update :: proc(cache: rawptr, filename: string) {
    panic("test_cache_update can't cache new files.")
}

test_cache_check :: proc(cache: rawptr, filename: string) -> Cache_Status {
    cache := cast(^Test_Cache) cache
    for rec in cache.recs {
        if rec.filename == filename {
            return rec.status
        }
    }
    return .Not_Exist
}

