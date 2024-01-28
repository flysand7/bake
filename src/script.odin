package bake

import "core:fmt"
import "core:strings"
import "core:reflect"

Builtin_Func :: #type proc (ctx: ^Ctx, args: []Value) -> Value

Value_Nil :: struct{}

Value :: union {
    i64,
    string,
    bool,
    Stmt_Func,
    Builtin_Func,
    []Value,
}

Env :: struct {
    parent: ^Env,
    scope: map[string]Value,
}

Ctx :: struct {
    tasks: [dynamic]Recipe,
    global_env: ^Env,
}

value_is_nil :: proc(v: Value) -> bool {
    return v == nil
}

value_is_int :: proc(v: Value) -> bool {
    if _, ok := v.(i64); ok {
        return true
    }
    return false
}

value_is_str :: proc(v: Value) -> bool {
    if _, ok := v.(string); ok {
        return true
    }
    return false
}

value_is_bool :: proc(v: Value) -> bool {
    if _, ok := v.(bool); ok {
        return true
    }
    return false
}

value_is_arr :: proc(v: Value) -> bool {
    if _, ok := v.([]Value); ok {
        return true
    }
    return false
}

value_is_func :: proc(v: Value) -> bool {
    if _, ok := v.(Stmt_Func); ok {
        return true
    }
    return false
}

value_is_builtin :: proc(v: Value) -> bool {
    if _, ok := v.(Builtin_Func); ok {
        return true
    }
    return false
}

value_is_any_func :: proc(v: Value) -> bool {
    return value_is_func(v) || value_is_builtin(v)
}

value_to_int :: proc(v: Value) -> (i64, bool) {
    if i, ok := v.(i64); ok {
        return i, true
    } else {
        return 0, false
    }
}

value_to_str :: proc(value: Value) -> (string, bool) {
    #partial switch v in value {
        case nil:    return "", true
        case bool:   return v? "true" : "false", true
        case i64:    return fmt.tprint(v), true
        case string: return v, true
        case: return "", false
    }
}

value_to_bool :: proc(value: Value) -> bool {
    #partial switch v in value {
        case nil: return false
        case bool: return v
        case i64: return v != 0
        case string: return v != ""
        case []Value: return len(v) != 0
        case: return true
    }
}

ctx_make :: proc() -> Ctx {
    global_env := env_make(nil)
    return {
        make([dynamic]Recipe),
        global_env,
    }
}

env_make :: proc(parent: ^Env) -> ^Env {
    env := new(Env)
    env.parent = parent
    env.scope = make(map[string]Value)
    if parent == nil {
        env_set(env, "print", cast(Builtin_Func) builtin_print)
        env_set(env, "cmd", cast(Builtin_Func) builtin_cmd)
        env_set(env, "len", cast(Builtin_Func) builtin_len)
        env_set(env, "bake-task", cast(Builtin_Func) builtin_task)
        env_set(env, "bake-build", cast(Builtin_Func) builtin_build)
    }
    return env
}

env_get :: proc(env: ^Env, name: string) -> Maybe(Value) {
    env := env
    for env != nil {
        if name in env.scope {
            return env.scope[name]
        }
        env = env.parent
    }
    return nil
}

env_set :: proc(env: ^Env, name: string, value: Value) {
    start_env := env
    env := start_env
    for env != nil {
        if name in env.scope {
            env.scope[name] = value
            return
        }
        env = env.parent
    }
    start_env.scope[name] = value
}

exec_stmts :: proc(stmts: []^Stmt) {
    ctx := ctx_make()
    env := env_make(nil)
    for stmt in stmts {
        exec_stmt(&ctx, env, stmt)
    }
}

exec_stmt :: proc(ctx: ^Ctx, env: ^Env, stmt: ^Stmt) {
    switch stmt in stmt.un {
        case Stmt_Expr:
            eval_expr(ctx, env, stmt.expr)
        case Stmt_Decl:
            value := eval_expr(ctx, env, stmt.value)
            env_set(env, stmt.name.name, value)
        case Stmt_If:
            val := eval_expr(ctx, env, stmt.cond)
            if v, ok := val.(bool); ok {
                exec_stmt(ctx, env, stmt.branch_t)
            } else {
                exec_stmt(ctx, env, stmt.branch_f)
            }
        case Stmt_For:
            for {
                val := eval_expr(ctx, env, stmt.cond)
                if value_to_bool(val) {
                    exec_stmt(ctx, env, stmt.body)
                } else {
                    break
                }
            }
        case Stmt_Func:
            env_set(env, stmt.name.name, stmt)
        case []^Stmt:
            env := env_make(env)
            for stmt in stmt {
                exec_stmt(ctx, env, stmt)
            }
    }
}

eval_expr :: proc(ctx: ^Ctx, env: ^Env, expr: ^Expr) -> Value {
    switch expr in expr.un {
        case Lit_String:
            return expr.value
        case Lit_Template:
            return eval_template(env, expr.value)
        case Lit_Int:
            return expr.value
        case Identifier:
            value := env_get(env, expr.name)
            if value, ok := value.?; ok {
                return value
            }
            panic("No value")
        case Expr_Unary:
            switch expr.op {
            }
            unreachable()
        case Expr_Binary:
            if expr.op == .Assign {
                if ident, ok := expr.lhs.un.(Identifier); ok {
                    new_value := eval_expr(ctx, env, expr.rhs)
                    env_set(env, ident.name, new_value)
                    after_set := env_get(env, ident.name)
                    return nil
                } else {
                    panic("Assign to non-expr")
                }
            }
            return eval_binary_op(expr.op, eval_expr(ctx, env, expr.lhs), eval_expr(ctx, env, expr.rhs))
        case Expr_Ternary:
            switch expr.op {
            }
            unreachable()
        case Expr_Call:
            evaluated_args := make([dynamic]Value)
            for arg in expr.args {
                append(&evaluated_args, eval_expr(ctx, env, arg))
            }
            mb_val := env_get(env, expr.fn.name)
            if val, ok := mb_val.?; ok {
                if fn, ok := val.(Stmt_Func); ok {
                    assert(len(expr.args) == len(fn.params), "Bad arg count")
                    env := env_make(ctx.global_env)
                    for p, i in fn.params {
                        env_set(env, p.name.name, evaluated_args[i])
                    }
                    stmts, stmts_ok := fn.body.un.([]^Stmt)
                    for stmt in stmts {
                        exec_stmt(ctx, env, stmt)
                    }
                    return nil
                } else if fn, ok := val.(Builtin_Func); ok {
                    return fn(ctx, evaluated_args[:])
                }
            }
        case Expr_Array:
            values := make([dynamic]Value)
            for e in expr.exprs {
                append(&values, eval_expr(ctx, env, e))
            }
            return values[:]
    }
    unreachable()
}

eval_binary_op :: proc(
    op: Binary_Op,
    lhs: Value,
    rhs: Value,
) -> Value {
    if value_is_any_func(lhs) || value_is_any_func(rhs) {
        panic("Functions don't support binary operations")
    }
    #partial switch op {
        case .Add:
            if value_is_int(lhs) && value_is_int(rhs) {
                return lhs.(i64) + rhs.(i64)
            } else if value_is_arr(lhs) && value_is_arr(rhs) {
                joined := make([dynamic]Value)
                for v in lhs.([]Value) {
                    append(&joined, v)
                }
                for v in rhs.([]Value) {
                    append(&joined, v)
                }
            } else {
                lhs_str, lhs_ok := value_to_str(lhs)
                rhs_str, rhs_ok := value_to_str(rhs)
                if !lhs_ok || !rhs_ok {
                    panic("Unable to add values of the provided types")
                }
                return fmt.tprint(lhs_str, rhs_str, sep="")
            }
        case .Div:
            if value_is_int(lhs) && value_is_int(rhs) {
                if rhs.(i64) == 0 {
                    panic("Division by zero")
                }
                return lhs.(i64) / rhs.(i64)
            } else if value_is_str(lhs) && value_is_str(rhs) {
                return fmt.tprint(lhs.(string), '/', rhs.(string), sep="")
            } else {
                panic("Divison of bad types")
            }
        case .Sub:
            if value_is_int(lhs) && value_is_int(rhs) {
                return rhs.(i64) - lhs.(i64)
            } else {
                panic("Subtraction of bad types")
            }
        case .Mul:
            if value_is_int(lhs) && value_is_int(rhs) {
                return lhs.(i64) * rhs.(i64)
            } else {
                panic("Multiplication of bad types")
            }
        case .Eq:
            if !value_is_nil(lhs) && !value_is_nil(rhs) {
                if value_is_int(lhs) && value_is_int(rhs) {
                    return lhs.(i64) == rhs.(i64)
                } if value_is_arr(lhs) && value_is_arr(rhs) {
                    lhs_arr := lhs.([]Value)
                    rhs_arr := rhs.([]Value)
                    if len(lhs_arr) != len(rhs_arr) {
                        return false
                    }
                    for i in 0 ..< len(lhs_arr) {
                        eq := eval_binary_op(.Eq, lhs_arr[i], rhs_arr[i])
                        if !eq.(bool) {
                            return false
                        }
                    }
                    return true
                } else if value_is_str(lhs) || value_is_str(rhs) {
                    lhs_s, lhs_ok := value_to_str(lhs)
                    rhs_s, rhs_ok := value_to_str(rhs)
                    if lhs_ok && rhs_ok {
                        return lhs_s == rhs_s
                    } else {
                        panic("Comparing bad types")
                    }
                } else {
                    panic("Comparing bad types")
                }
            } else {
                return value_is_nil(lhs) == value_is_nil(rhs)
            }
        case .Ne:
            if !value_is_nil(lhs) && !value_is_nil(rhs) {
                if value_is_int(lhs) && value_is_int(rhs) {
                    return lhs.(i64) != rhs.(i64)
                } if value_is_arr(lhs) && value_is_arr(rhs) {
                    lhs_arr := lhs.([]Value)
                    rhs_arr := rhs.([]Value)
                    if len(lhs_arr) != len(rhs_arr) {
                        return true
                    }
                    for i in 0 ..< len(lhs_arr) {
                        eq := eval_binary_op(.Eq, lhs_arr[i], rhs_arr[i])
                        if !eq.(bool) {
                            return true
                        }
                    }
                    return false
                } else if value_is_str(lhs) || value_is_str(rhs) {
                    lhs_s, lhs_ok := value_to_str(lhs)
                    rhs_s, rhs_ok := value_to_str(rhs)
                    if lhs_ok && rhs_ok {
                        return lhs_s != rhs_s
                    } else {
                        panic("Comparing bad types")
                    }
                } else {
                    panic("Comparing bad types")
                }
            } else {
                return value_is_nil(lhs) != value_is_nil(rhs)
            }
        case .Ge:
            if value_is_nil(lhs) || value_is_nil(rhs) {
                return false
            }
            if value_is_int(lhs) && value_is_int(rhs) {
                return lhs.(i64) >= rhs.(i64)
            } else if value_is_str(lhs) || value_is_str(rhs) {
                lhs_s, lhs_ok := value_to_str(lhs)
                rhs_s, rhs_ok := value_to_str(rhs)
                if lhs_ok && rhs_ok {
                    return lhs_s >= rhs_s
                } else {
                    panic("Comparing bad types")
                }
            } else {
                panic("Comparing bad types")
            }
        case .Gt:
            if value_is_nil(lhs) || value_is_nil(rhs) {
                return false
            }
            if value_is_int(lhs) && value_is_int(rhs) {
                return lhs.(i64) > rhs.(i64)
            } else if value_is_str(lhs) || value_is_str(rhs) {
                lhs_s, lhs_ok := value_to_str(lhs)
                rhs_s, rhs_ok := value_to_str(rhs)
                if lhs_ok && rhs_ok {
                    return lhs_s > rhs_s
                } else {
                    panic("Comparing bad types")
                }
            } else {
                panic("Comparing bad types")
            }
        case .Le:
            if value_is_nil(lhs) || value_is_nil(rhs) {
                return false
            }
            if value_is_int(lhs) && value_is_int(rhs) {
                return lhs.(i64) <= rhs.(i64)
            } else if value_is_str(lhs) || value_is_str(rhs) {
                lhs_s, lhs_ok := value_to_str(lhs)
                rhs_s, rhs_ok := value_to_str(rhs)
                if lhs_ok && rhs_ok {
                    return lhs_s <= rhs_s
                } else {
                    panic("Comparing bad types")
                }
            } else {
                panic("Comparing bad types")
            }
        case .Lt:
            if value_is_nil(lhs) || value_is_nil(rhs) {
                return false
            }
            if value_is_int(lhs) && value_is_int(rhs) {
                return lhs.(i64) < rhs.(i64)
            } else if value_is_str(lhs) || value_is_str(rhs) {
                lhs_s, lhs_ok := value_to_str(lhs)
                rhs_s, rhs_ok := value_to_str(rhs)
                if lhs_ok && rhs_ok {
                    return lhs_s < rhs_s
                } else {
                    panic("Comparing bad types")
                }
            } else {
                panic("Comparing bad types")
            }
        case .Nvl:
            if !value_is_nil(lhs) {
                return lhs
            } else {
                return rhs
            }
        case .Subscript:
            arr := lhs.([]Value)
            if !value_is_int(rhs) {
                panic("Only can subscript arrays with integers")
            }
            index := rhs.(i64)
            if index < 0 && auto_cast len(arr) <= index {
                panic("Out of bounds array access")
            }
            return arr[index]
    }
    unreachable()
}

eval_template :: proc(env: ^Env, str: string) -> string {
    sb := strings.builder_make()
    for i := 0; i < len(str); {
        if str[i] != '$' {
            strings.write_byte(&sb, str[i])
            i += 1
        } else {
            i += 1
            if str[i] != '{' {
                continue
            }
            i += 1
            start_idx := i
            for str[i] != '}' {
                i += 1
            }
            end_idx := i
            i += 1
            mb_val := env_get(env, str[start_idx:end_idx])
            if val, ok := mb_val.?; ok {
                strings.write_string(&sb, fmt.tprint(val))
            }
        }
    }
    return strings.to_string(sb)
}
