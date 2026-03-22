package bake

import "core:testing"

// An empty set of recipes should produce an empty execution plan.
@(test)
test_empty_recipe_set :: proc(t: ^testing.T) {
    recipes := []Recipe {}
    plan := build_execution_plan(nil, recipes, []string {})
    testing.expect_value(t, len(plan), 0)
}

// If the recipe set contains any commands that don't produce an output, these
// recipes should be discarded.
@(test)
test_discard_unused_recipes_1 :: proc(t: ^testing.T) {
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin test src" },
            inputs = []string { "src/**" },
            outputs = nil,
        },
    }
    plan := build_execution_plan(nil, recipes, []string {})
    testing.expect_value(t, len(plan), 0)
}

// If the recipe set contains one changed output, that recipe should be
// executed.
@(test)
test_one_output_pass :: proc(t: ^testing.T) {
    cache := test_cache_make([]Test_Cache_Rec {
        Test_Cache_Rec {
            filename = "src/main.odin",
            status = .Updated,
        }
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []string { "src/main.odin" },
            outputs = []string { "test.exe" },
        },
    }
    plan := build_execution_plan(&cache, recipes, {"test.exe"})
    testing.expect_value(t, len(plan), 1)
}

// If the recipe set contains one unchanged output, that recipe should be
// discarded.
@(test)
test_one_output_discard :: proc(t: ^testing.T) {
    cache := test_cache_make([]Test_Cache_Rec {
        Test_Cache_Rec {
            filename = "src/main.odin",
            status = .Unchanged,
        },
        Test_Cache_Rec {
            filename = "test.exe",
            status = .Unchanged,
        },
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []string { "src/main.odin" },
            outputs = []string { "test.exe" },
        },
    }
    plan := build_execution_plan(&cache, recipes, {"test.exe"})
    testing.expect_value(t, len(plan), 0)
}

// If the recipe set contains a changed input, but the output is unchanged,
// that recipe should be executed.
@(test)
test_changed_input_pass :: proc(t: ^testing.T) {
    cache := test_cache_make([]Test_Cache_Rec {
        Test_Cache_Rec {
            filename = "src/main.odin",
            status = .Updated,
        },
        Test_Cache_Rec {
            filename = "test.exe",
            status = .Unchanged,
        },
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []string { "src/main.odin" },
            outputs = []string { "test.exe" },
        },
    }
    plan := build_execution_plan(&cache, recipes, {"test.exe"})
    testing.expect_value(t, len(plan), 1)
}

// If the recipe set contains a changed output, that recipe should be executed,
// regardless of whether the input is changed or not.
@(test)
test_changed_output_pass :: proc(t: ^testing.T) {
    cache := test_cache_make([]Test_Cache_Rec {
        Test_Cache_Rec {
            filename = "src/main.odin",
            status = .Unchanged,
        },
        Test_Cache_Rec {
            filename = "test.exe",
            status = .Updated,
        },
    })
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "odin build src -o:test.exe" },
            inputs = []string { "src/main.odin" },
            outputs = []string { "test.exe" },
        },
    }
    plan := build_execution_plan(&cache, recipes, {"test.exe"})
    testing.expect_value(t, len(plan), 1)
}

// If the intermediate output is changed, both, the step that is producing the
// intermediate output and the step that is using that output should be re-run.
@(test)
test_intermediate_output_change_pass :: proc(t: ^testing.T) {
    cache := test_cache_make([]Test_Cache_Rec {
        Test_Cache_Rec { "src/main.odin", .Unchanged },
        Test_Cache_Rec { "test.o", .Updated },
        Test_Cache_Rec { "test.a", .Unchanged },
    })
    // Note(flysand): These are intentionally out of order to also test the
    // sorting that build_execution_plan should perform.
    recipes := []Recipe {
        Recipe {
            cmds = []Cmd { "llvm-ar xrv test.a test.o" },
            inputs = []string { "test.o" },
            outputs = []string { "test.a" },
        },
        Recipe {
            cmds = []Cmd { "odin build src -o:test.o -build-mode:obj" },
            inputs = []string { "src/main.odin" },
            outputs = []string { "test.o" },
        },
    }
    plan := build_execution_plan(&cache, recipes, { "test.a" })
    testing.expect_value(t, len(plan), 2)
    testing.expect_value(t, plan[0].cmds[0].(string), "odin build src -o:test.o -build-mode:obj")
    testing.expect_value(t, plan[1].cmds[0].(string), "llvm-ar xrv test.a test.o")
}
