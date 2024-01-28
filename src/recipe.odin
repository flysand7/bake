package bake

import "core:slice"
import "core:os"
import "core:fmt"

Recipe :: struct {
    cmds: []Cmd,
    inputs: []string,
    outputs: []string,
}

Dep_Node :: struct {
    task: Recipe,
    node_depn: [dynamic]^Dep_Node,
    leaf_depn: [dynamic]string,
    intr_depn: [dynamic]string,
}

Dep_Tree :: struct {
    roots: [dynamic]^Dep_Node,
}

Exec_Task :: struct {
    cmds: []Cmd,
    leaf_inputs: []File,
    intr_inputs: []string,
    outputs: []string,
}

dep_tree_make :: proc() -> Dep_Tree {
    return {
        roots = make([dynamic]^Dep_Node),
    }
}

find_task_producing_file :: proc(tasks: []Recipe, file: string) -> (Recipe, bool) {
    for task in tasks {
        for output in task.outputs {
            if file == output {
                return task, true
            }
        }
    }
    return {}, false
}

dep_node_for_task :: proc(tasks: []Recipe, task: Recipe) -> ^Dep_Node {
    dn := new(Dep_Node)
    dn.task = task
    for input in task.inputs {
        depn_task, ok := find_task_producing_file(tasks, input)
        if !ok {
            append(&dn.leaf_depn, input)
        } else {
            append(&dn.node_depn, dep_node_for_task(tasks, depn_task))
            append(&dn.intr_depn, input)
        }
    }
    return dn
}

dep_tree_from_tasks :: proc(tasks: []Recipe, targets: []string) -> Dep_Tree {
    dt := dep_tree_make()
    for target in targets {
        task, ok := find_task_producing_file(tasks, target)
        if !ok {
            panic("No task produces a desired target")
        }
        append(&dt.roots, dep_node_for_task(tasks, task))
    }
    return dt
}

dep_tree_toposort :: proc(dt: Dep_Tree) -> []Exec_Task {
    dep_tree_walk :: proc(arr: ^[dynamic]^Dep_Node, dn: ^Dep_Node) {
        for node in dn.node_depn {
            dep_tree_walk(arr, node)
        }
        append(arr, dn)
    }
    nodes := make([dynamic]^Dep_Node)
    for root in dt.roots {
        dep_tree_walk(&nodes, root)
    }
    tasks := make([]Exec_Task, len(nodes))
    for n, i in nodes {
        leaf_files := make([]File, len(n.leaf_depn))
        for filename, j in n.leaf_depn {
            ok := false
            leaf_files[j], ok = file_make(filename)
            if !ok {
                panic("File dependency not found")
            }
        }
        tasks[i] = Exec_Task {
            cmds = n.task.cmds,
            leaf_inputs = leaf_files,
            intr_inputs = n.intr_depn[:],
            outputs = n.task.outputs[:],
        }
    }
    return tasks
}

build_files :: proc(tasks: []Recipe, files: []string) {
    cache := cache_open()
    out_cache := cache_make()
    dt := dep_tree_from_tasks(tasks, files)
    exec_tasks := dep_tree_toposort(dt)
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
            assert(file_ok, "File not produced by previous task")
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
