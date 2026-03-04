package bake

import "core:os"
import "core:path/filepath"

BAKEFILE_NOT_FOUND :: ""

find_bakefile :: proc() -> (string) {
    cwd, err := os.get_working_directory(context.allocator)
    assert(err == nil)
    return walk_parent_directories(cwd)
}

@(private="file")
walk_parent_directories :: proc(dir_path: string) -> string {
    dir_path, dir_path_err := filepath.clean(dir_path, context.allocator)
    assert(dir_path_err == nil)
    if dir_path == "/" {
        return BAKEFILE_NOT_FOUND
    }
    if !os.is_dir(dir_path) {
        assert(false)
        return BAKEFILE_NOT_FOUND
    }
    bakefile_path, bakefile_path_err := filepath.join([]string{dir_path, "bakefile"}, context.allocator)
    assert(bakefile_path_err == nil)
    if os.exists(bakefile_path) {
        return bakefile_path
    } else {
        parent_dir, parent_dir_err := filepath.join([]string{dir_path, ".."}, context.allocator)
        assert(parent_dir_err == nil)
        return walk_parent_directories(parent_dir)
    }
}
