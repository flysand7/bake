package bake

import "core:slice"
import "core:os"

Cache_Rec :: struct {
    id: File_Id,
    ts: u64,
}

Cache :: struct {
    recs: [dynamic]Cache_Rec,
}

Cache_Status :: enum {
    Unchanged,
    Not_Exist,
    Updated,
}

cache_open :: proc() -> Cache {
    bytes, err := os.read_entire_file(".bake-cache", context.allocator)
    if err != nil {
        return Cache {}
    }
    recs := make([dynamic]Cache_Rec)
    prev_recs := slice.reinterpret([]Cache_Rec, bytes)
    append(&recs, ..prev_recs)
    return Cache {
        recs = recs,
    }
}

cache_make :: proc() -> Cache {
    return Cache {
        recs = make([dynamic]Cache_Rec),
    }
}

cache_file :: proc(cache: ^Cache, filename: string) {
    id, file_ts, ok := file_get_id(filename)
    // TODO(flysand, 2026-03-22): Check if the file is already cached. Currently
    // the cache is created either for reading or writing, but not both. At some
    // point would want to move onto one cache for both reading AND writing, which
    // will avoid unnecessary looping over the DAG.
    append(&cache.recs, Cache_Rec { id, file_ts })
}

cache_check :: proc(cache: ^Cache, filename: string) -> Cache_Status {
    rec: Cache_Rec = ---
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

cache_write :: proc(cache: ^Cache) {
    bytes := slice.reinterpret([]u8, cache.recs[:])
    perm := os.Permissions {
        .Read_Other, .Read_Group, .Read_User, .Write_User
    }
    err := os.write_entire_file(".bake-cache", bytes, perm)
    assert(err == nil)
}

