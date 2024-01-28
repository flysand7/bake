package bake

import "core:fmt"

builtin_print :: proc(ctx: ^Ctx, args: []Value) -> Value {
    for arg in args {
        fmt.print(arg, sep="")
    }
    return nil
}

builtin_cmd :: proc(ctx: ^Ctx, args: []Value) -> Value {
    strings := make([dynamic]string)
    for arg in args {
        str, ok := value_to_str(arg)
        if !ok {
            panic("cmd(arr) One of the values doesn't cast to string")
        }
        append(&strings, str)
    }
    code, err := run_cmd(strings[:])
    if err != nil {
        return fmt.tprint(err)
    } else {
        return cast(i64) code
    }
}

builtin_len :: proc(ctx: ^Ctx, args: []Value) -> Value {
    if len(args) != 1 {
        panic("len(arr) takes one argument")
    }
    arg := args[0]
    if value_is_arr(arg) {
        return cast(i64) len(arg.([]Value))
    } else if value_is_str(arg) {
        return cast(i64) len(arg.(string))
    } else {
        panic("len() expects either a string or an array")
    }
    return nil
}

builtin_task :: proc(ctx: ^Ctx, args: []Value) -> Value {
    if len(args) != 3 {
        panic("task(cmd, in, out) function takes three arguments")
    }
    cmd := args[0]
    inputs := args[1]
    outputs := args[2]
    if !value_is_arr(cmd) {
        panic("task(cmd,,): parameter must be an array")
    }
    if !value_is_arr(inputs) {
        panic("task(,in,): parameter must be an array")
    }
    if !value_is_arr(outputs) {
        panic("task(,,out): parameter must be an array")
    }
    cmd_arrs := make([dynamic]Cmd)
    inputs_strs := make([dynamic]string)
    outputs_strs := make([dynamic]string)
    for arr, idx in cmd.([]Value) {
        if !value_is_arr(arr) {
            panic("task(cmd): Parameter must be an array of arrays")
        }
        append(&cmd_arrs, make([]string, len(arr.([]Value))))
        for el, i in arr.([]Value) {
            str, ok := value_to_str(el)
            if !ok {
                panic("task(cmd): Array element not castable to string")
            }
            cmd_arrs[idx].([]string)[i] = str
        }
    }
    for el in inputs.([]Value) {
        str, ok := value_to_str(el)
        if !ok {
            panic("task(in): Array element not castable to string")
        }
        append(&inputs_strs, str)
    }
    for el in outputs.([]Value) {
        str, ok := value_to_str(el)
        if !ok {
            panic("task(in): Array element not castable to string")
        }
        append(&outputs_strs, str)
    }
    append(&ctx.tasks, Recipe {
        cmds = cmd_arrs[:],
        inputs = inputs_strs[:],
        outputs = outputs_strs[:],
    })
    return nil
}

builtin_build :: proc(ctx: ^Ctx, args: []Value) -> Value {
    if len(args) != 1 {
        panic("build(files) takes one argument")
    }
    files := args[0]
    if !value_is_arr(files) {
        panic("build(files) takes an array")
    }
    files_arr := files.([]Value)
    files_strs := make([dynamic]string)
    for v in files_arr {
        str, ok := value_to_str(v)
        if !ok {
            panic("build(files) found a value no assignable to strings")
        }
        append(&files_strs, str)
    }
    build_files(ctx.tasks[:], files_strs[:])
    return nil
}
