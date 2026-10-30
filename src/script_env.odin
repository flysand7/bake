package bake

Env_Entry :: struct {
    mutable: bool,
    value: Value,
}

Env :: struct {
    parent: ^Env,
    scope: map[string]Env_Entry,
}

Env_Error :: enum {
    None,
    Invalid_Mutation,
    Builtin_Shadowing,
    Shadowing,
    Undeclared_Variable,
}

env_make :: proc(parent: ^Env) -> ^Env {
    env := new(Env)
    env.parent = parent
    env.scope = make(map[string]Env_Entry)
    if parent == nil {
        for builtin_name, builtin in builtin_map {
            env.scope[builtin_name] = Env_Entry {
                mutable = false,
                value = builtin,
            }
        }
    }
    return env
}

env_get :: proc(env: ^Env, name: string) -> Maybe(Value) {
    env := env
    for env != nil {
        if name in env.scope {
            return env.scope[name].value
        }
        env = env.parent
    }
    return nil
}

env_set :: proc(env: ^Env, name: string, value: Value) -> Env_Error {
    env := env
    start_env := env
    for env != nil {
        if name in env.scope {
            break
        }
        env = env.parent
    }
    if env == nil {
        return .Undeclared_Variable
    }
    entry := env.scope[name]
    if !entry.mutable {
        return .Invalid_Mutation
    }
    entry.value = value
    env.scope[name] = entry
    return nil
}

env_declare :: proc(env: ^Env, name: string, value: Value, mutable: bool) -> Env_Error {
    for builtin_name in builtin_map {
        if name == builtin_name {
            return .Builtin_Shadowing
        }
    }
    if name in env.scope {
        return .Shadowing
    }
    env.scope[name] = Env_Entry {
        value = value,
        mutable = mutable,
    }
    return nil
}
