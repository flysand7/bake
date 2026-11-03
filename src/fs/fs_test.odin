package bake_fs

import "core:testing"

@(test)
test_path_interning :: proc(t: ^testing.T) {
    fs := make_fs(context.allocator)
    f_1 := register_file(&fs, "/", "file.txt", context.allocator)
    f_2 := register_file(&fs, "/", "file.txt", context.allocator)
    testing.expect(t, f_1 == f_2, "Same file should equal to itself")
    f_3 := register_file(&fs, "/", "boo.txt", context.allocator)
    testing.expect(t, f_1 != f_3, "Different files shouldn't be equal")
}

@(test)
test_path_normalization :: proc(t: ^testing.T) {
    fs := make_fs(context.allocator)
    f_1 := register_file(&fs, "/", "file.txt", context.allocator)
    f_2 := register_file(&fs, "/", "../file.txt", context.allocator)
    testing.expect(t, f_1 == f_2, "Going back a directory from root should return root")
    f_3 := register_file(&fs, "/app", "file.txt", context.allocator)
    f_4 := register_file(&fs, "/", "app/file.txt", context.allocator)
    f_5 := register_file(&fs, "/", "./app/file.txt", context.allocator)
    testing.expect(t, f_3 == f_4, "Path relative to cwd should be equal to absolute path")
    testing.expect(t, f_3 == f_5, "Path starting with './' shouldn't be any different from path without it")
    f_6 := register_file(&fs, "C:/", "./app/file.txt")
    testing.expect(t, f_3 != f_6, "Path with drive specification and without should be different paths")
    f_7 := register_file(&fs, "/boo", "/app/file.txt")
    testing.expect(t, f_3 != f_7, "Absolute path should be handled regardless of the supplied working directory")
}
