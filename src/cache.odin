package bake

import "core:slice"
import "core:os"

Cache :: struct {
    files: [dynamic]File,
}

cache_open :: proc() -> Cache {
    bytes, ok := os.read_entire_file(".bake-cache")
    if !ok {
        return Cache {}
    }
    files := slice.reinterpret([]File, bytes)
    dfiles := make([dynamic]File)
    append(&dfiles, ..files)
    return Cache {
        files = dfiles,
    }
}

cache_make :: proc() -> Cache {
    return Cache {
        files = make([dynamic]File),
    }
}

cache_add :: proc(cache: ^Cache, file: File) {
    append(&cache.files, file)
}

cache_find :: proc(cache: ^Cache, file: File) -> (File, bool) {
    for cf in cache.files {
        if file_is_same(file, cf) {
            return cf, true
        }
    }
    return {}, false
}

cache_write :: proc(cache: ^Cache) {
    bytes := slice.reinterpret([]u8, cache.files[:])
    ok := os.write_entire_file(".bake-cache", bytes, true)
    assert(ok)
}

