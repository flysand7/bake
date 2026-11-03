package bake

import "fs"
import "cache"

import "core:testing"

// An empty set of recipes should produce an empty execution plan.
@(test)
test_empty_recipe_set :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    recipes := []Recipe {}
    plan := build_execution_plan(&f, nil, recipes, []fs.File_Id {})
    testing.expect_value(t, len(plan), 0)
}

// If the recipe set contains any commands that don't produce an output, these
// recipes should be discarded.
@(test)
test_discard_unused_recipes_1 :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    i1 := fs.register_file(&f, "/app", "src/main.odin")
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin test src" },
            inputs = []fs.File_Id { i1 },
            outputs = nil,
        },
    }
    plan := build_execution_plan(&f, nil, recipes, []fs.File_Id {})
    testing.expect_value(t, len(plan), 0)
}

// If the recipe set contains one changed output, that recipe should be
// executed.
@(test)
test_one_output_pass :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    i1 := fs.register_file(&f, "/app", "src/main.odin", context.allocator)
    o1 := fs.register_file(&f, "/app", "test.exe", context.allocator)
    c := cache.test_cache_make([]cache.Test_Cache_Rec {
        cache.Test_Cache_Rec {
            filename = "/app/src/main.odin",
            status = .Updated,
        }
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []fs.File_Id { i1 },
            outputs = []fs.File_Id { o1 },
        },
    }
    plan := build_execution_plan(&f, &c, recipes, { o1 })
    testing.expect_value(t, len(plan), 1)
}

// If the recipe set contains one unchanged output, that recipe should be
// discarded.
@(test)
test_one_output_discard :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    i1 := fs.register_file(&f, "/app", "src/main.odin", context.allocator)
    o1 := fs.register_file(&f, "/app", "test.exe", context.allocator)
    c := cache.test_cache_make([]cache.Test_Cache_Rec {
        cache.Test_Cache_Rec {
            filename = "/app/src/main.odin",
            status = .Unchanged,
        },
        cache.Test_Cache_Rec {
            filename = "/app/test.exe",
            status = .Unchanged,
        },
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []fs.File_Id { i1 },
            outputs = []fs.File_Id { o1 },
        },
    }
    plan := build_execution_plan(&f, &c, recipes, { o1 })
    testing.expect_value(t, len(plan), 0)
}

// If the recipe set contains a changed input, but the output is unchanged,
// that recipe should be executed.
@(test)
test_changed_input_pass :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    i1 := fs.register_file(&f, "/app", "src/main.odin", context.allocator)
    o1 := fs.register_file(&f, "/app", "test.exe", context.allocator)
    c := cache.test_cache_make([]cache.Test_Cache_Rec {
        cache.Test_Cache_Rec {
            filename = "/app/src/main.odin",
            status = .Updated,
        },
        cache.Test_Cache_Rec {
            filename = "/app/test.exe",
            status = .Unchanged,
        },
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []fs.File_Id { i1 },
            outputs = []fs.File_Id { o1 },
        },
    }
    plan := build_execution_plan(&f, &c, recipes, { o1 })
    testing.expect_value(t, len(plan), 1)
}

// If the recipe set contains a changed output, that recipe should be executed,
// regardless of whether the input is changed or not.
@(test)
test_changed_output_pass :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    i1 := fs.register_file(&f, "/app", "src/main.odin", context.allocator)
    o1 := fs.register_file(&f, "/app", "test.exe", context.allocator)
    c := cache.test_cache_make([]cache.Test_Cache_Rec {
        cache.Test_Cache_Rec {
            filename = "/app/src/main.odin",
            status = .Unchanged,
        },
        cache.Test_Cache_Rec {
            filename = "/app/test.exe",
            status = .Updated,
        },
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []fs.File_Id { i1 },
            outputs = []fs.File_Id { o1 },
        },
    }
    plan := build_execution_plan(&f, &c, recipes, { o1 })
    testing.expect_value(t, len(plan), 1)
}

// If the intermediate output is changed, both, the step that is producing the
// intermediate output and the step that is using that output should be re-run.
@(test)
test_intermediate_output_change_pass :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    i1 := fs.register_file(&f, "/app", "src/main.odin", context.allocator)
    o1 := fs.register_file(&f, "/app", "test.o", context.allocator)
    o2 := fs.register_file(&f, "/app", "test.a", context.allocator)
    c := cache.test_cache_make([]cache.Test_Cache_Rec {
        cache.Test_Cache_Rec { "/app/src/main.odin", .Unchanged },
        cache.Test_Cache_Rec { "/app/test.o", .Updated },
        cache.Test_Cache_Rec { "/app/test.a", .Unchanged },
    })
    // Note(flysand): These are intentionally out of order to also test the
    // sorting that build_execution_plan should perform.
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "llvm-ar xrv test.a test.o" },
            inputs = []fs.File_Id { o1 },
            outputs = []fs.File_Id { o2 },
        },
        Recipe {
            cmds = []Cmd { "odin build src -o:test.o -build-mode:obj" },
            inputs = []fs.File_Id { i1 },
            outputs = []fs.File_Id { o1 },
        },
    }
    plan := build_execution_plan(&f, &c, recipes, { o2 })
    testing.expect_value(t, len(plan), 2)
    testing.expect_value(t, plan[0].cmds[0].(string), "odin build src -o:test.o -build-mode:obj")
    testing.expect_value(t, plan[1].cmds[0].(string), "llvm-ar xrv test.a test.o")
}

// If the input has changed, the downstream outputs must be invalidated and
// recompiled.
@(test)
test_downstream_inputs_invalidation :: proc(t: ^testing.T) {
    f := fs.make_fs(context.allocator)
    f_a := fs.register_file(&f, "/app", "a.txt", context.allocator)
    f_b := fs.register_file(&f, "/app", "b.txt", context.allocator)
    f_c := fs.register_file(&f, "/app", "c.txt", context.allocator)
    c := cache.test_cache_make([]cache.Test_Cache_Rec {
        cache.Test_Cache_Rec { "/app/a.txt", .Updated },
        cache.Test_Cache_Rec { "/app/b.txt", .Unchanged },
        cache.Test_Cache_Rec { "/app/c.txt", .Unchanged },
    })
    // Note(flysand): These are intentionally out of order to also test the
    // sorting that build_execution_plan should perform.
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "a > b"},
            inputs = []fs.File_Id { f_a },
            outputs = []fs.File_Id { f_b },
        },
        Recipe {
            cmds = []Cmd { "b > c" },
            inputs = []fs.File_Id { f_b },
            outputs = []fs.File_Id { f_c },
        },
    }
    plan := build_execution_plan(&f, &c, recipes, { f_c })
    testing.expect_value(t, len(plan), 2)
    testing.expect_value(t, plan[0].cmds[0].(string), "a > b")
    testing.expect_value(t, plan[1].cmds[0].(string), "b > c")
}
