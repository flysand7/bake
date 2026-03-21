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
    // The `cache_file` procedure implementation.
    _impl_file: proc(ctx: rawptr, filename: string),
    // The `cache_check` procedure implementation.
    _impl_check: proc(ctx: rawptr, filename: string) -> Cache_Status,
}

/*
Cache a file.

This should record the requested file's information inside the cache.
*/
cache_file :: proc(ic: ^Cache, filename: string) {
    ic._impl_file(ic.ctx, filename)
}

/*
Check file state.

This should return information about whether the file is recorded inside the
cache and whether it was updated or not.
*/
cache_check :: proc(ic: ^Cache, filename: string) -> Cache_Status {
    return ic._impl_check(ic.ctx, filename)
}

Fs_Cache_Rec :: struct {
    id: File_Id,
    ts: u64,
}

Fs_Cache :: struct {
    recs: [dynamic]Fs_Cache_Rec,
}

fs_cache_open :: proc() -> Cache {
    bytes, err := os.read_entire_file(".bake-cache", context.allocator)
    cache := new(Fs_Cache)
    cache.recs = make([dynamic]Fs_Cache_Rec)
    if err != nil {
        // If the .bake-cache file doesn't exist, this behaves like cache_make.
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

fs_cache_file :: proc(cache: rawptr, filename: string) {
    cache := cast(^Fs_Cache) cache
    id, file_ts, ok := file_get_id(filename)
    // TODO(flysand, 2026-03-22): Check if the file is already cached. Currently
    // the cache is created either for reading or writing, but not both. At some
    // point would want to move onto one cache for both reading AND writing, which
    // will avoid unnecessary looping over the DAG.
    append(&cache.recs, Fs_Cache_Rec { id, file_ts })
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
        // TODO(flysand, 2026-03-22): This could also update the cache record
        // inside the cache records maybe?
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
    err := os.write_entire_file(".bake-cache", bytes, perm)
    assert(err == nil)
}

_fs_cache_impl :: proc(fs_cache: ^Fs_Cache) -> Cache {
    return Cache {
        ctx = fs_cache,
        _impl_file = fs_cache_file,
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
        _impl_file = test_cache_file,
        _impl_check = test_cache_check,
    }
}

test_cache_file :: proc(cache: rawptr, filename: string) {
    panic("test_cache_file can't cache new files.")
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

