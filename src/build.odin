package bake

import "core:os"
import "core:fmt"

Builtin_Cmd :: struct {
    execute: #type proc([]string) -> int,
    args: []string,
}

Cmd :: union {
    Builtin_Cmd,
    []string,
    string,
}

/*
Recipe

Recipes determine a way of obtaining a set of output files by executing a set of
input commands. The targets can be obtained if all of the input files exist and
the command is executed.
*/
Recipe :: struct {
    cmds: []Cmd,
    inputs: []string,
    outputs: []string,
}

/*
Recipe node in a dependency DAG

This struct is a node in the directed acyclic graph (DAG) of the dependency
tree. The DAG is built out of recipes, where each recipe with a set of input
files can depend on other recipes that provide some of these files as outputs.

The input files for the node are split into two categories:
1. The leaf dependencies - these files are expected to always exist in the
   project and may not altered by the build system. These files are typically
   the source code and other project files that exist prior to the first run
   of the build system.
2. The intermediate dependencies - these are files that are produced by one
   recipe but needed as an input for another recipe. These files are typically
   build artefacts.
*/
Recipe_Node :: struct {
    recipe: Recipe,
    dependencies: [dynamic]^Recipe_Node,
    leaf_dependencies: [dynamic]string,
    intr_dependencies: [dynamic]string,
}

/*
The directed acyclic graph (DAG) of recipe dependencies
*/
Recipe_DAG :: struct {
    roots: [dynamic]^Recipe_Node,
}

/*
A task to be executed

This defines a specific task to be executed. Each task specifies a command to be
executed and a set of inputs and outputs to that command. The files are being
kept track of in order to interface with the cache -- to see, if the task needs
to be executed at all. If the inputs are unchanged, the outputs will too
(provided the command is deterministic).
*/
Exec_Task :: struct {
    cmds: []Cmd,
    leaf_inputs: []File,
    intr_inputs: []string,
    outputs: []string,
}

find_recipe_for_target :: proc(recipes: []Recipe, target: string) -> (Recipe, bool) {
    for r in recipes {
        for output in r.outputs {
            if target == output {
                return r, true
            }
        }
    }
    return {}, false
}

dep_node_for_recipe :: proc(recipes: []Recipe, recipe: Recipe) -> ^Recipe_Node {
    dn := new(Recipe_Node)
    dn.recipe = recipe
    for input in recipe.inputs {
        depn_task, ok := find_recipe_for_target(recipes, input)
        if !ok {
            append(&dn.leaf_dependencies, input)
        } else {
            append(&dn.dependencies, dep_node_for_recipe(recipes, depn_task))
            append(&dn.intr_dependencies, input)
        }
    }
    return dn
}

dependency_dag_build :: proc(recipes: []Recipe, targets: []string) -> Recipe_DAG {
    dt := Recipe_DAG {
        roots = make([dynamic]^Recipe_Node),
    }
    for target in targets {
        recipe, ok := find_recipe_for_target(recipes, target)
        if !ok {
            panic("No recipe produces a desired target")
        }
        append(&dt.roots, dep_node_for_recipe(recipes, recipe))
    }
    return dt
}

dependency_dag_toposort :: proc(dt: Recipe_DAG) -> []Exec_Task {
    dep_tree_walk :: proc(arr: ^[dynamic]^Recipe_Node, dn: ^Recipe_Node) {
        for node in dn.dependencies {
            dep_tree_walk(arr, node)
        }
        append(arr, dn)
    }
    nodes := make([dynamic]^Recipe_Node)
    for root in dt.roots {
        dep_tree_walk(&nodes, root)
    }
    tasks := make([]Exec_Task, len(nodes))
    for n, i in nodes {
        leaf_files := make([]File, len(n.leaf_dependencies))
        for filename, j in n.leaf_dependencies {
            ok := false
            leaf_files[j], ok = file_make(filename)
            if !ok {
                panic("File dependency not found")
            }
        }
        tasks[i] = Exec_Task {
            cmds = n.recipe.cmds,
            leaf_inputs = leaf_files,
            intr_inputs = n.intr_dependencies[:],
            outputs = n.recipe.outputs[:],
        }
    }
    return tasks
}

build_files :: proc(recipes: []Recipe, files: []string) {
    cache := cache_open()
    out_cache := cache_make()
    dt := dependency_dag_build(recipes, files)
    exec_tasks := dependency_dag_toposort(dt)
    had_errors := false
    for task, idx in exec_tasks {
        cached := true
        for file in task.leaf_inputs {
            old_file, ok := cache_find(&cache, file)
            this_cached := ok && old_file.write_time == file.write_time
            if !this_cached {
                cached = false
            }
        }
        intr_inputs := make([dynamic]File)
        for filename in task.intr_inputs {
            file, file_ok := file_make(filename)
            append(&intr_inputs, file)
            assert(file_ok, "File not produced by previous recipe")
            old_file, cache_ok := cache_find(&cache, file)
            this_cached := cache_ok && old_file.write_time == file.write_time
            if !this_cached {
                cached = false
            }
        }
        for output in task.outputs {
            if !os.exists(output) {
                cached = false
            }
        }
        fmt.printf("%s[%d/%d]: %v\n", cached?"[CACHED]":"", idx+1, len(exec_tasks), task.cmds)
        if !cached {
            for cmd in task.cmds {
                code, err := run_cmd(cmd)
                if err != nil {
                    fmt.eprintf("Failed to run command: %v\n", cmd)
                    if err == .Executable_Not_Found {
                        fmt.eprintf("  Specified executable not found.\n")
                    }
                    os.exit(1)
                }
                if code != 0 {
                    fmt.printf("Command %v failed with error code: %d\n", cmd, code)
                    had_errors := true
                    break
                }
            }
            for out in task.outputs {
                file, ok := file_make(out)
                assert(ok, "Command doesn't produce the specified output")
                cache_add(&out_cache, file)
            }
        }
        for file in task.leaf_inputs {
            cache_add(&out_cache, file)
        }
        for file in intr_inputs {
            cache_add(&out_cache, file)
        }
    }
    cache_write(&out_cache)
}

