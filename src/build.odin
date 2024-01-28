package bake

Builtin_Cmd :: struct {
    execute: #type proc([]string) -> int,
    args: []string,
}

Cmd :: union {
    Builtin_Cmd,
    []string,
    string,
}

File_Target :: struct {
    filename: string,
}

Target :: union {
    File_Target,
}

