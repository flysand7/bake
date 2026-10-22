package bake

import "core:strings"
import "core:os"
import "core:fmt"
import "core:reflect"

import "cli"

Script_Error_Type :: enum {
    Lex_Error,
    Parse_Error,
    Runtime_Error,
}

Script_Error_Code :: enum {
    // Lexing errors.
    Unknown_Escape_Sequence = 1,
    Invalid_Number = 2,
    Unterminated_Template_Literal = 3,
    Unterminated_String_Literal = 4,
    Unexpected_Character = 5,
    // Parsing errors.
    Unexpected_Token = 6,
    Expected_Token = 7,
    Unterminated_Token = 8,
    Invalid_Func_Attribute = 9,
    // Runtime errors.
    Builtin_Shadowing = 10,
    Shadowing = 11,
    Control_Flow_Outside_Loops = 12,
    Undeclared_Identifier = 13,
    Invalid_Assignment_Target = 14,
    Argument_Count_Mismatch = 15,
    Call_Non_Func = 16,
    Binary_Op_Not_Applicable = 17,
    Division_By_Zero = 18,
    Subscript_Invalid_Type = 19,
    Subscript_Not_Applicable = 20,
    Out_Of_Bounds = 21,
    Dict_Key_Missing = 22,
}

Script_Error_Data :: union {
    rune,
    string,
    int,
    typeid,
    Token_Operator,
    Binary_Op,
}

Script_Error :: struct {
    bakefile_path: string,
    bakefile_text: string,
    type: Script_Error_Type,
    code: Script_Error_Code,
    loc: Loc,
    data: []Script_Error_Data,
}

error_message_map: [Script_Error_Code]string = {
    .Unknown_Escape_Sequence = "Invalid escape sequence '$0'",
    .Invalid_Number = "Invalid number literal '$0'",
    .Unterminated_Template_Literal = "Template literal wasn't terminated",
    .Unterminated_String_Literal = "String literal wasn't terminated",
    .Unexpected_Character = "Unexpected character '$0'",
    .Unexpected_Token = "Unexpected token '$0'",
    .Expected_Token = "Expected token '$0', instead got '$1'",
    .Unterminated_Token = "Token '$0' was not terminated by '$1'",
    .Invalid_Func_Attribute = "Invalid function attribute '$0'; Expected 'cli'",
    .Builtin_Shadowing = "Shadowing of a built-in '$0' is not allowed",
    .Shadowing = "Variable '$0' shadows another variable in the same scope",
    .Control_Flow_Outside_Loops = "Unexpected control-flow statement '$0' outside of a loop",
    .Undeclared_Identifier = "Identifier '$0' was not declared",
    .Invalid_Assignment_Target = "Expression is not a valid assignment target",
    .Argument_Count_Mismatch = "Function '$0' expected '$1' arguments, '$2' given",
    .Call_Non_Func = "Value of type '$0' is not callable",
    .Binary_Op_Not_Applicable = "Operation '$0' is not applicable for types '$0' and '$1'",
    .Division_By_Zero = "Division by zero",
    .Subscript_Invalid_Type = "Subscript operation expects a number, but '$0' was found",
    .Subscript_Not_Applicable = "Subscript operation is not applicable for value of type '$0'",
    .Out_Of_Bounds = "Out of bounds access of index $0 for array of length $1",
    .Dict_Key_Missing = "Key '$0' is missing in a dictionary",
}

format_error :: proc(e: Script_Error, a := context.allocator) -> string {
    sb := strings.builder_make(a)
    if cli.config.stderr_color {
        fmt.sbprint(&sb, cli.COLOR_ERROR)
    }
    error_fmt: string = ---
    switch e.type {
        case .Lex_Error:     error_fmt = "Error[L%04d]@%s(%d,%d): "
        case .Parse_Error:   error_fmt = "Error[P%04d]@%s(%d,%d): "
        case .Runtime_Error: error_fmt = "Error[R%04d]@%s(%d,%d): "
    }
    line, col := loc_extract_line_col(e.loc.offs, e.bakefile_text)
    fmt.sbprintf(&sb, error_fmt, e.code, e.bakefile_path, line, col)
    error_message_format := error_message_map[e.code]
    for i := 0; i < len(error_message_format); i += 1 {
        if error_message_format[i] != '$' {
            strings.write_byte(&sb, error_message_format[i])
        } else {
            i += 1
            data_idx := int(error_message_format[i] - '0')
            data := e.data[data_idx]
            switch d in data {
            case rune:
                strings.write_rune(&sb, d)
            case string:
                strings.write_string(&sb, d)
            case int:
                strings.write_int(&sb, d)
            case typeid:
                fmt.sbprintf(&sb, "%v", d)
            case Token_Operator:
                strings.write_string(&sb, operator_names[d])
            case Binary_Op:
                strings.write_string(&sb, binary_op_names[d])
            }
        }
    }
    if cli.config.stderr_color {
        fmt.sbprint(&sb, cli.COLOR_RESET)
    }
    return strings.to_string(sb)
}

loc_extract_line_col :: proc(pos: int, text: string) -> (int, int) {
    newline_count := 0
    newline_offs := -1
    for offs in 0 ..< len(text) {
        if offs >= pos {
            break
        }
        if text[offs] == '\n' {
            newline_count += 1
            newline_offs = offs
        }
    }
    line := newline_count + 1
    offs := pos - newline_offs
    return line, offs
}

error_unknown_escape_sequence :: proc(p: ^Parser, esc: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = esc
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Lex_Error,
        code = .Unknown_Escape_Sequence,
        loc = Loc {
            offs = p.idx,
            width = 1,
        },
        data = data,
    }
}

error_invalid_number :: proc(p: ^Parser, s: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = s
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Lex_Error,
        code = .Invalid_Number,
        loc = Loc {
            offs = p.idx,
            width = len(s),
        },
        data = data,
    }
}

error_unterminated_template_literal :: proc(p: ^Parser, start: Loc) -> Script_Error {
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Lex_Error,
        code = .Unterminated_Template_Literal,
        loc = start,
        data = nil,
    }
}

error_unterminated_string_literal :: proc(p: ^Parser, start: Loc) -> Script_Error {
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Lex_Error,
        code = .Unterminated_String_Literal,
        loc = start,
        data = nil,
    }
}

error_unexpected_character :: proc(p: ^Parser, ch: rune) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = ch
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Lex_Error,
        code = .Unexpected_Character,
        loc = Loc {
            offs = p.idx,
            width = 1,
        },
        data = data,
    }
}

error_unexpected_token :: proc(p: ^Parser, user_token: Token) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    #partial switch v in user_token.un {
    case Token_Operator:
        data[0] = v
    case:
        data[0] = typeid_of(type_of(user_token.un))
    }
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Parse_Error,
        code = .Unexpected_Token,
        loc = user_token.loc,
        data = data,
    }
}

@(private="file")
error_expected_token_un :: proc(p: ^Parser, expected: typeid, got: Token) -> Script_Error {
    data := make([]Script_Error_Data, 2)
    data[0] = expected
    #partial switch v in got.un {
    case Token_Operator:
        data[1] = v
    case:
        data[1] = reflect.union_variant_typeid(got.un)
    }
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Parse_Error,
        code = .Expected_Token,
        loc = got.loc,
        data = data,
    }
}

@(private="file")
error_expected_token_op :: proc(p: ^Parser, expected: Token_Operator, got: Token) -> Script_Error {
    data := make([]Script_Error_Data, 2)
    data[0] = expected
    #partial switch v in got.un {
    case Token_Operator:
        data[1] = v
    case:
        data[1] = reflect.union_variant_typeid(got.un)
    }
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Parse_Error,
        code = .Expected_Token,
        loc = got.loc,
        data = data,
    }
}

error_expected_token :: proc {
    error_expected_token_un,
    error_expected_token_op,
}

error_unterminated_token :: proc(p: ^Parser, start: Token, end: Token_Operator) -> Script_Error {
    data := make([]Script_Error_Data, 2)
    data[0] = start.un.(Token_Operator)
    data[1] = end
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Parse_Error,
        code = .Unexpected_Token,
        loc = start.loc,
        data = data,
    }
}

error_invalid_func_attribute :: proc(p: ^Parser, attr: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = attr
    return {
        bakefile_path = p.file_path,
        bakefile_text = p.file_text,
        type = .Parse_Error,
        code = .Invalid_Func_Attribute,
        loc = p.token.loc,
        data = data,
    }
}

error_builtin_shadowing :: proc(ctx: ^Ctx, loc: Loc, name: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = name
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Builtin_Shadowing,
        loc = loc,
        data = data,
    }
}

error_shadowing :: proc(ctx: ^Ctx, loc: Loc, name: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = name
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Shadowing,
        loc = loc,
        data = data,
    }
}

error_control_flow_outside_loop :: proc(ctx: ^Ctx, loc: Loc, name: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = name
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Control_Flow_Outside_Loops,
        loc = loc,
        data = data,
    }
}

error_undeclared_identifier :: proc(ctx: ^Ctx, loc: Loc, name: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = name
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Undeclared_Identifier,
        loc = loc,
        data = data,
    }
}

error_invalid_assignment_target :: proc(ctx: ^Ctx, loc: Loc) -> Script_Error {
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Invalid_Assignment_Target,
        loc = loc,
        data = nil,
    }
}

error_argument_count_mismatch :: proc(ctx: ^Ctx, loc: Loc, f: string, expected: int, given: int) -> Script_Error {
    data := make([]Script_Error_Data, 3)
    data[0] = f
    data[1] = expected
    data[2] = given
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Argument_Count_Mismatch,
        loc = loc,
        data = data,
    }
}

error_call_non_func :: proc(ctx: ^Ctx, loc: Loc, v: Value) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = value_type(v)
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Call_Non_Func,
        loc = loc,
        data = data,
    }
}

error_binary_op_not_applicable :: proc(ctx: ^Ctx, loc: Loc, op: Binary_Op, t1: Value, t2: Value) -> Script_Error {
    data := make([]Script_Error_Data, 3)
    data[0] = op
    data[1] = value_type(t1)
    data[2] = value_type(t2)
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Binary_Op_Not_Applicable,
        loc = loc,
        data = data,
    }
}

error_division_by_zero :: proc(ctx: ^Ctx, loc: Loc) -> Script_Error {
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Division_By_Zero,
        loc = loc,
        data = nil,
    }
}

error_subscript_invalid_type :: proc(ctx: ^Ctx, loc: Loc, v: Value) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = value_type(v)
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Subscript_Invalid_Type,
        loc = loc,
        data = data,
    }
}

error_subscript_not_applicable :: proc(ctx: ^Ctx, loc: Loc, v: Value) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = value_type(v)
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Subscript_Not_Applicable,
        loc = loc,
        data = data,
    }
}

error_out_of_bounds :: proc(ctx: ^Ctx, loc: Loc, index: int, len: int) -> Script_Error {
    data := make([]Script_Error_Data, 2)
    data[0] = index
    data[1] = len
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Out_Of_Bounds,
        loc = loc,
        data = data,
    }
}

error_dict_key_missing :: proc(ctx: ^Ctx, loc: Loc, key: string) -> Script_Error {
    data := make([]Script_Error_Data, 1)
    data[0] = key
    return {
        bakefile_path = ctx.file_path,
        bakefile_text = ctx.file_text,
        type = .Runtime_Error,
        code = .Dict_Key_Missing,
        loc = loc,
        data = data,
    }
}

script_error :: proc(e: Script_Error, a := context.allocator) -> ! {
    s := format_error(e, a)
    fmt.eprintln(s)
    os.exit(1)
}
