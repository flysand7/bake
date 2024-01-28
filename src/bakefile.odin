package bake

import "core:os"
import "core:os/os2"
import "core:path/filepath"

BAKEFILE_NOT_FOUND :: ""

find_bakefile :: proc() -> (string) {
    cwd := os.get_current_directory()
    return walk_parent_directories(cwd)
}

@(private="file")
walk_parent_directories :: proc(dir_path: string) -> string {
    dir_path := filepath.clean(dir_path)
    if dir_path == "/" {
        return BAKEFILE_NOT_FOUND
    }
    if !os2.is_dir(dir_path) {
        assert(false)
        return BAKEFILE_NOT_FOUND
    }
    bakefile_path := filepath.join([]string{dir_path, "bakefile"})
    if os.exists(bakefile_path) {
        return bakefile_path
    } else {
        parent_dir := filepath.join([]string{dir_path, ".."})
        return walk_parent_directories(parent_dir)
    }
}
