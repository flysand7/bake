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
