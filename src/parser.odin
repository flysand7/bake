package bake

import "core:fmt"
import "base:intrinsics"
import "core:unicode/utf8"
import "core:strconv"
import "core:strings"
import "core:reflect"

Unary_Op :: enum {
    Not,
}

Binary_Op :: enum {
    Add,
    Sub,
    Mul,
    Div,
    Nvl,
    Assign,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    And,
    Or,
    Xor,
    Implies,
    Subscript,
    Member,
}

binary_op_names: [Binary_Op]string = {
    .Add = "+",
    .Sub = "-",
    .Mul = "*",
    .Div = "/",
    .Nvl = "??",
    .Assign = "=",
    .Eq = "==",
    .Ne = "!=",
    .Le = "<=",
    .Ge = ">=",
    .Lt = "<",
    .Gt = ">",
    .And = "and",
    .Or = "or",
    .Xor = "xor",
    .Implies = "implies",
    .Subscript = "[]",
    .Member = ".",
}

Ternary_Op :: enum {
}

Loc :: struct {
    offs:  int,
    width: int,
}

Lit_String :: struct {
    value: string,
}

Lit_Template :: struct {
    value: string,
}

Lit_Int :: struct {
    value: i64,
}

Identifier :: struct {
    name: string,
}

Expr_Unary :: struct {
    op: Unary_Op,
    expr: ^Expr,
}

Expr_Binary :: struct {
    op: Binary_Op,
    lhs: ^Expr,
    rhs: ^Expr,
}

Expr_Ternary :: struct {
    op: Ternary_Op,
    lhs: ^Expr,
    mhs: ^Expr,
    rhs: ^Expr,
}

Expr_Call :: struct {
    fn: ^Expr,
    args: []^Expr,
}

Expr_Array :: struct {
    exprs: []^Expr,
}

Expr_Dict :: struct {
    names: []Identifier,
    values: []^Expr,
}

Lit_Nil :: struct {}

Expr_Un :: union {
    Identifier,
    Lit_Nil,
    Lit_String,
    Lit_Template,
    Lit_Int,
    Expr_Array,
    Expr_Dict,
    Expr_Unary,
    Expr_Binary,
    Expr_Ternary,
    Expr_Call,
}

Expr :: struct {
    loc: Loc,
    un: Expr_Un,
}

Stmt_Expr :: struct {
    expr: ^Expr,
}

Stmt_If :: struct {
    cond: ^Expr,
    branch_t: ^Stmt,
    branch_f: ^Stmt,
}

Stmt_Return :: struct {
    expr: ^Expr,
}

Stmt_Break :: struct {}

Stmt_Continue :: struct {}

Stmt_For :: struct {
    cond: ^Expr,
    body: ^Stmt,
}

Stmt_Decl :: struct {
    mutable: bool,
    name: Identifier,
    value: ^Expr,
}

Func_Param :: struct {
    loc: Loc,
    name: Identifier,
}

Stmt_Func :: struct {
    is_cli: bool,
    name: Identifier,
    params: []Func_Param,
    body: ^Stmt,
}

Stmt_Un :: union {
    Stmt_Expr,
    Stmt_Decl,
    Stmt_If,
    Stmt_For,
    Stmt_Func,
    Stmt_Return,
    Stmt_Break,
    Stmt_Continue,
    []^Stmt,
}

Stmt :: struct {
    loc: Loc,
    un: Stmt_Un,
}

Token_Operator :: enum {
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Nvl,
    Comma,
    Dot,
    Assign,
    Arrow,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Ln,
}

operator_names: [Token_Operator]string = {
    .Add = "+",
    .Sub = "-",
    .Mul = "*",
    .Div = "/",
    .Not = "!",
    .Eq = "==",
    .Ne = "!=",
    .Lt = "<",
    .Gt = ">",
    .Le = "<=",
    .Ge = ">=",
    .Nvl = "??",
    .Comma = ",",
    .Dot = ".",
    .Assign = "=",
    .Arrow = "->",
    .LParen = "(",
    .RParen = ")",
    .LBracket = "{",
    .RBracket = "}",
    .LBrace = "[",
    .RBrace = "]",
    .Ln = "newline",
}

Token_Un :: union {
    Lit_Int,
    Lit_String,
    Lit_Template,
    Token_Operator,
    Identifier,
}

Token :: struct {
    loc: Loc,
    un: Token_Un,
}

Parser :: struct {
    file_path: string,
    file_text: string,
    idx: int,
    last_ch: rune,
    token: Token,
}

parser_make :: proc(file_path: string, file_text: string) -> Parser {
    p := Parser {
        file_path = file_path,
        file_text = file_text,
        idx = 0,
    }
    parser_char_next(&p)
    parser_token_next(&p)
    return p
}

parser_char_next :: proc(p: ^Parser) {
    if len(p.file_text[p.idx:]) == 0 {
        p.last_ch = 0
    }
    ch, sz := utf8.decode_rune_in_string(p.file_text[p.idx:])
    if ch == utf8.RUNE_ERROR {
        ch = 0
    }
    p.idx += sz
    p.last_ch = ch
}

parser_char_is :: proc(p: ^Parser, ch: rune) -> bool {
    if p.last_ch == ch {
        return true
    }
    return false
}

parser_char_match :: proc(p: ^Parser, ch: rune) -> bool {
    if p.last_ch == ch {
        parser_char_next(p)
        return true
    }
    return false
}

parser_char_is_range :: proc(p: ^Parser, start, end: rune) -> bool {
    if start <= p.last_ch && p.last_ch <= end {
        return true
    }
    return false
}

parser_char_match_range :: proc(p: ^Parser, start, end: rune) -> bool {
    if start <= p.last_ch && p.last_ch <= end {
        parser_char_next(p)
        return true
    }
    return false
}

parser_match_string_escape :: proc(p: ^Parser) -> rune {
    assert(parser_char_is(p, '\\'))
    parser_char_next(p)
    if parser_char_match(p, 'e') {
        return '\e'
    } else if parser_char_match(p, 'a') {
        return '\a'
    } else if parser_char_match(p, 'b') {
        return '\b'
    } else if parser_char_match(p, 'r') {
        return '\r'
    } else if parser_char_match(p, 'n') {
        return '\n'
    } else if parser_char_match(p, 't') {
        return '\t'
    } else if parser_char_match(p, 'v') {
        return '\v'
    } else if parser_char_match(p, 'u') {
        script_error(error_unknown_escape_sequence(p, "\\u"))
    } else if parser_char_match(p, 'x') {
        script_error(error_unknown_escape_sequence(p, "\\x"))
    }
    return 0
}

parser_token_next :: proc(p: ^Parser) {
    // Skip whitespace and comments.
    for {
        if  parser_char_is(p, ' ') || parser_char_is(p, '\t') || parser_char_is(p, '\r') {
            parser_char_next(p)
        } else if parser_char_is(p, '#') {
            for !parser_char_is(p, '\n') && !parser_char_is(p, 0) {
                parser_char_next(p)
            }
        } else {
            break
        }
    }
    t := Token {
        loc = Loc {
            offs = p.idx-1,
        },
    }
    if parser_char_is_range(p, 'a', 'z') || parser_char_is_range(p, 'A', 'Z') || parser_char_is(p, '_') {
        for parser_char_is_range(p, 'a', 'z') ||
            parser_char_is_range(p, 'A', 'Z') ||
            parser_char_is_range(p, '0', '9') ||
            parser_char_is(p, '_') ||
            parser_char_is(p, '-') \
        {
            parser_char_next(p)
        }
        t.un = Identifier {
            name = p.file_text[t.loc.offs:p.idx-1],
        }
    } else if parser_char_is_range(p, '0', '9') {
        for parser_char_is_range(p, '0', '9') ||
            parser_char_is(p, '_') \
        {
            parser_char_next(p)
        }
        num_str := p.file_text[t.loc.offs:p.idx-1]
        value, ok := strconv.parse_i64(num_str, 10)
        if !ok {
            script_error(error_invalid_number(p, num_str))
        }
        t.un = Lit_Int {
            value = value,
        }
    } else if parser_char_is(p, '"') {
        str := strings.builder_make()
        start := p.idx
        parser_char_next(p)
        for !parser_char_is(p, '"') {
            if parser_char_is(p, 0) {
                script_error(error_unterminated_template_literal(p, Loc { start, 1 }))
            }
            if parser_char_is(p, '\\') {
                escape_start := p.idx
                escaped := parser_match_string_escape(p)
                if escaped != 0 {
                    strings.write_rune(&str, escaped)
                } else {
                    script_error(error_unknown_escape_sequence(p, p.file_text[start:p.idx+1]))
                }
            } else {
                strings.write_rune(&str, p.last_ch)
                parser_char_next(p)
            }
        }
        ok := parser_char_match(p, '"')
        assert(ok)
        t.un = Lit_Template {
            value = strings.to_string(str),
        }
    } else if parser_char_is(p, '\'') {
        str := strings.builder_make()
        start := p.idx
        parser_char_next(p)
        for !parser_char_is(p, '\'') {
            if parser_char_is(p, 0) {
                script_error(error_unterminated_string_literal(p, Loc { p.idx, 1 }))
            }
            if parser_char_is(p, '\\') {
                escape_start := p.idx
                escaped := parser_match_string_escape(p)
                if escaped != 0 {
                    strings.write_rune(&str, escaped)
                } else {
                    script_error(error_unknown_escape_sequence(p, p.file_text[escape_start:p.idx+1]))
                }
            } else {
                strings.write_rune(&str, p.last_ch)
                parser_char_next(p)
            }
        }
        ok := parser_char_match(p, '\'')
        assert(ok)
        t.un = Lit_String {
            value = strings.to_string(str),
        }
    } else if parser_char_match(p, '=') {
        t.un = Token_Operator.Assign
        if parser_char_match(p, '=') {
            t.un = Token_Operator.Eq
        }
    } else if parser_char_match(p, '<') {
        t.un = Token_Operator.Lt
        if parser_char_match(p, '=') {
            t.un = Token_Operator.Le
        }
    } else if parser_char_match(p, '>') {
        t.un = Token_Operator.Gt
        if parser_char_match(p, '=') {
            t.un = Token_Operator.Ge
        }
    } else if parser_char_match(p, '!') {
        t.un = Token_Operator.Not
        if parser_char_match(p, '=') {
            t.un = Token_Operator.Ne
        }
    } else if parser_char_match(p, '(') {
        t.un = Token_Operator.LParen
    } else if parser_char_match(p, ')') {
        t.un = Token_Operator.RParen
    } else if parser_char_match(p, '[') {
        t.un = Token_Operator.LBracket
    } else if parser_char_match(p, ']') {
        t.un = Token_Operator.RBracket
    } else if parser_char_match(p, '{') {
        t.un = Token_Operator.LBrace
    } else if parser_char_match(p, '}') {
        t.un = Token_Operator.RBrace
    } else if parser_char_match(p, ',') {
        t.un = Token_Operator.Comma
    } else if parser_char_match(p, '.') {
        t.un = Token_Operator.Dot
    } else if parser_char_match(p, '+') {
        t.un = Token_Operator.Add
    } else if parser_char_match(p, '*') {
        t.un = Token_Operator.Mul
    } else if parser_char_match(p, '/') {
        t.un = Token_Operator.Div
    } else if parser_char_match(p, '-') {
        if parser_char_match(p, '>') {
            t.un = Token_Operator.Sub
        }
    } else if parser_char_match(p, '?') {
        if parser_char_match(p, '?') {
            t.un = Token_Operator.Nvl
        }
    } else if parser_char_match(p, '\n') {
        t.un = Token_Operator.Ln
    } else if parser_char_is(p, 0) {
        t.un = nil
    } else {
        script_error(error_unexpected_character(p, p.last_ch))
    }
    t.loc.width = p.idx - t.loc.offs
    p.token = t
}

@(require_results)
parser_token_is :: proc(p: ^Parser, $T: typeid) -> (T, bool) {
    if v, ok := p.token.un.(T); ok {
        return v, true
    }
    return {}, false
}

@(require_results)
parser_token_match :: proc(p: ^Parser, $T: typeid) -> (T, bool) {
    if v, ok := p.token.un.(T); ok {
        parser_token_next(p)
        return v, true
    }
    return {}, false
}

parser_token_expect :: proc(p: ^Parser, $T: typeid) -> T {
    if t, ok  := p.token.un.(T); ok {
        parser_token_next(p)
        return t
    }
    script_error(error_expected_token(p, T, p.token))
}

@(require_results)
parser_op_is :: proc(p: ^Parser, op: Token_Operator) -> bool {
    if tok, ok := p.token.un.(Token_Operator); ok {
        if tok == op {
            return true
        }
    }
    return false
}

@(require_results)
parser_op_match :: proc(p: ^Parser, op: Token_Operator) -> bool {
    if tok, ok := p.token.un.(Token_Operator); ok {
        if tok == op {
            parser_token_next(p)
            return true
        }
    }
    return false
}

parser_op_expect :: proc(p: ^Parser, op: Token_Operator) {
    if tok, ok := p.token.un.(Token_Operator); ok {
        if tok == op {
            parser_token_next(p)
            return
        }
    }
    script_error(error_expected_token(p, op, p.token))
}

merge_locs :: proc(locs: ..Loc) -> Loc {
    locs := locs[:]
    min_offs := locs[0].offs
    max_offs := locs[0].offs + locs[0].width
    for l in locs[1:] {
        if l.offs < min_offs {
            min_offs = l.offs
        }
        if l.offs + l.width > max_offs {
            max_offs = l.offs + l.width
        }
    }
    return Loc {
        offs = min_offs,
        width = max_offs - min_offs,
    }
}

expr_make :: proc(l: Loc, un: Expr_Un) -> ^Expr {
    expr := new(Expr)
    expr.loc = l
    expr.un = un
    return expr
}

expr_make_unary_op :: proc(op: Unary_Op, e: ^Expr) -> ^Expr {
    return expr_make(e.loc, Expr_Unary {
        op = op,
        expr = e,
    })
}

expr_make_binary_op :: proc(op: Binary_Op, lhs, rhs: ^Expr) -> ^Expr {
    return expr_make(
        merge_locs(lhs.loc, rhs.loc),
        Expr_Binary {
            op = op,
            lhs = lhs,
            rhs = rhs,
        },
    )
}

expr_make_ternary_op :: proc(op: Ternary_Op, lhs, mhs, rhs: ^Expr) -> ^Expr {
    return expr_make(
        merge_locs(lhs.loc, mhs.loc, rhs.loc),
        Expr_Ternary {
            op = op,
            lhs = lhs,
            mhs = mhs,
            rhs = rhs,
        },
    )
}

stmt_make :: proc(loc: Loc, un: Stmt_Un) -> ^Stmt {
    stmt := new(Stmt)
    stmt.loc = loc
    stmt.un = un
    return stmt
}

skip_newline :: proc(p: ^Parser) {
    for parser_op_is(p, .Ln) {
        parser_token_next(p)
    }
}

expect_1_skip_newlines :: proc(p: ^Parser) {
    parser_op_expect(p, .Ln)
    for parser_op_is(p, .Ln) {
        parser_token_next(p)
    }
}

parse_expr_simple :: proc(p: ^Parser) -> ^Expr {
    expr := cast(^Expr) nil
    if lit, ok := parser_token_is(p, Lit_Int); ok {
        expr = expr_make(p.token.loc, lit)
    } else if lit, ok := parser_token_is(p, Lit_String); ok {
        expr = expr_make(p.token.loc, lit)
    } else if lit, ok := parser_token_is(p, Lit_Template); ok {
        expr = expr_make(p.token.loc, lit)
    } else if ident, ok := parser_token_is(p, Identifier); ok {
        ident_loc := p.token.loc
        parser_token_next(p)
        if ident.name == "true" {
            return expr_make(ident_loc, Lit_Int{1})
        } else if ident.name == "false" {
            return expr_make(ident_loc, Lit_Int{0})
        } else if ident.name == "nil" {
            return expr_make(ident_loc, Lit_Nil{})
        } else {
            expr = expr_make(ident_loc, ident)
        }
        return expr
    } else if parser_op_is(p, .LBracket) {
        lbracket_token := p.token
        parser_op_expect(p, .LBracket)
        skip_newline(p)
        exprs := make([dynamic]^Expr)
        loc := p.token.loc
        parse_next_ok := true
        for !parser_op_is(p, .RBracket) {
            if !parse_next_ok {
                script_error(error_expected_token(p, Token_Operator.Comma, p.token))
            }
            if p.token.un == nil {
                script_error(error_unterminated_token(p, lbracket_token, Token_Operator.RBracket))
            }
            append(&exprs, parse_expr(p))
            if !parser_op_match(p, .Comma) {
                parse_next_ok = false
            }
            skip_newline(p)
        }
        loc = merge_locs(loc, p.token.loc)
        parser_op_expect(p, .RBracket)
        return expr_make(loc, Expr_Array{
            exprs = exprs[:],
        })
    } else if parser_op_is(p, .LBrace) {
        lbrace := p.token
        parser_op_expect(p, .LBrace)
        skip_newline(p)
        names := make([dynamic]Identifier)
        exprs := make([dynamic]^Expr)
        loc := p.token.loc
        parse_next_ok := true
        for !parser_op_is(p, .RBrace) {
            if !parse_next_ok {
                script_error(error_expected_token(p, Token_Operator.Comma, p.token))
            }
            if p.token.un == nil {
                script_error(error_unterminated_token(p, lbrace, Token_Operator.RBrace))
            }
            if ident, ok := parser_token_is(p, Identifier); ok {
                parser_token_next(p)
                append(&names, ident)
                parser_op_expect(p, .Assign)
                append(&exprs, parse_expr(p))
                if !parser_op_match(p, .Comma) {
                    parse_next_ok = false
                }
                skip_newline(p)
            } else {
                script_error(error_expected_token(p, Identifier, p.token))
            }
        }
        loc = merge_locs(loc, p.token.loc)
        parser_op_expect(p, .RBrace)
        return expr_make(loc, Expr_Dict {
            names = names[:],
            values = exprs[:],
        })
    } else if parser_op_match(p, .LParen) {
        skip_newline(p)
        defer parser_op_expect(p, .RParen)
        return parse_expr(p)
    } else {
        script_error(error_unexpected_token(p, p.token))
    }
    parser_token_next(p)
    return expr
}

parse_expr_not_so_simple :: proc (p: ^Parser) -> ^Expr {
    lhs := parse_expr_simple(p)
    for {
        if parser_op_match(p, .Dot) {
            ident_loc := p.token.loc
            if ident, ok := parser_token_match(p, Identifier); ok {
                lhs = expr_make_binary_op(.Member, lhs, expr_make(ident_loc, Lit_String{ident.name}))
            } else {
                script_error(error_expected_token(p, Identifier, p.token))
            }
        } else if parser_op_is(p, .LParen) {
            lparen_token := p.token
            parser_token_next(p)
            skip_newline(p)
            args := make([dynamic]^Expr)
            loc := p.token.loc
            parse_next_ok := true
            for !parser_op_is(p, .RParen) {
                if !parse_next_ok {
                    script_error(error_expected_token(p, Token_Operator.Comma, p.token))
                }
                if p.token.un == nil {
                    script_error(error_unterminated_token(p, lparen_token, Token_Operator.RParen))
                }
                append(&args, parse_expr(p))
                if !parser_op_match(p, .Comma) {
                    parse_next_ok = false
                }
                skip_newline(p)
            }
            loc = merge_locs(loc, p.token.loc)
            parser_op_expect(p, .RParen)
            lhs = expr_make(loc, Expr_Call {
                fn = lhs,
                args = args[:],
            })
        } else if parser_op_match(p, .LBracket) {
            loc := p.token.loc
            rhs := parse_expr(p)
            loc = merge_locs(loc, p.token.loc)
            parser_op_expect(p, .RBracket)
            lhs = expr_make(loc, Expr_Binary {
                op = .Subscript,
                lhs = lhs,
                rhs = rhs,
            })
        } else {
            break
        }
    }
    return lhs
}

parse_expr_unary :: proc(p: ^Parser) -> ^Expr {
    if parser_op_match(p, .Not) {
        expr := parse_expr_not_so_simple(p)
        return expr_make_unary_op(.Not, expr)
    }
    return parse_expr_not_so_simple(p)
}

parse_expr0 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr_unary(p)
    for parser_op_is(p, .Nvl) {
        t := p.token.un.(Token_Operator)
        parser_token_next(p)
        skip_newline(p)
        rhs := parse_expr_unary(p)
        lhs = expr_make_binary_op(Binary_Op(.Nvl), lhs, rhs)
    }
    return lhs
}

parse_expr1 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr0(p)
    for parser_op_is(p, .Mul) || parser_op_is(p, .Div) {
        t := p.token.un.(Token_Operator)
        parser_token_next(p)
        skip_newline(p)
        rhs := parse_expr0(p)
        bop := Binary_Op(nil)
        if t == .Mul {
            bop = .Mul
        } else if t == .Div {
            bop = .Div
        }
        lhs = expr_make_binary_op(bop, lhs, rhs)
    }
    return lhs
}

parse_expr2 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr1(p)
    for parser_op_is(p, .Add) || parser_op_is(p, .Sub) {
        t := p.token.un.(Token_Operator)
        parser_token_next(p)
        skip_newline(p)
        rhs := parse_expr1(p)
        bop := Binary_Op(nil)
        if t == .Add {
            bop = .Add
        } else if t == .Sub {
            bop = .Sub
        }
        lhs = expr_make_binary_op(bop, lhs, rhs)
    }
    return lhs
}

parse_expr3 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr2(p)
    for parser_op_is(p, .Eq) || parser_op_is(p, .Ne) || parser_op_is(p, .Lt) || parser_op_is(p, .Gt) || parser_op_is(p, .Le) || parser_op_is(p, .Ge) {
        t := p.token.un.(Token_Operator)
        parser_token_next(p)
        skip_newline(p)
        rhs := parse_expr2(p)
        bop := Binary_Op(nil)
        #partial switch t {
            case .Eq: bop = .Eq
            case .Ne: bop = .Ne
            case .Lt: bop = .Lt
            case .Gt: bop = .Gt
            case .Le: bop = .Le
            case .Ge: bop = .Ge
        }
        lhs = expr_make_binary_op(bop, lhs, rhs)
    }
    return lhs
}

parse_expr4 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr3(p)
    for {
        if ident, ok := parser_token_is(p, Identifier); ok && ident.name == "and" {
            parser_token_next(p)
            skip_newline(p)
            rhs := parse_expr3(p)
            lhs = expr_make_binary_op(.And, lhs, rhs)
        } else {
            break
        }
    }
    return lhs
}

parse_expr5 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr4(p)
    parse_ops: for {
        if ident, ok := parser_token_is(p, Identifier); ok {
            bop := Binary_Op(nil)
            switch ident.name {
            case "or":  bop = .Or
            case "xor": bop = .Xor
            case: break parse_ops
            }
            parser_token_next(p)
            skip_newline(p)
            rhs := parse_expr4(p)
            lhs = expr_make_binary_op(bop, lhs, rhs)
        } else {
            break
        }
    }
    return lhs
}

parse_expr6 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr5(p)
    parse_ops: for {
        if ident, ok := parser_token_is(p, Identifier); ok && ident.name == "implies" {
            parser_token_next(p)
            skip_newline(p)
            rhs := parse_expr5(p)
            lhs = expr_make_binary_op(.Implies, lhs, rhs)
        } else {
            break
        }
    }
    return lhs
}

parse_expr :: proc(p: ^Parser) -> ^Expr {
    return parse_expr6(p)
}

parse_expr_toplevel :: proc(p: ^Parser) -> ^Expr {
    expr := parse_expr(p)
    if parser_op_match(p, .Assign) {
        skip_newline(p)
        expr = expr_make_binary_op(
            .Assign,
            expr,
            parse_expr(p),
        )
    }
    return expr
}

parse_stmt :: proc(p: ^Parser) -> ^Stmt {
    tok := p.token
    if ident, ok := tok.un.(Identifier); ok {
        kw := ident.name
        switch kw {
        case "if":
            parser_token_next(p)
            cond := parse_expr(p)
            branch_t := parse_stmt(p)
            branch_f := cast(^Stmt) (nil)
            if ident, ok := parser_token_is(p, Identifier); ok && ident.name == "else" {
                parser_token_next(p)
                branch_f = parse_stmt(p)
            }
            return stmt_make(cond.loc, Stmt_If {
                cond = cond,
                branch_t = branch_t,
                branch_f = branch_f,
            })
        case "for":
            parser_token_next(p)
            cond := parse_expr(p)
            stmts := parse_stmt_block(p)
            expect_1_skip_newlines(p)
            return stmt_make(merge_locs(cond.loc, stmts.loc), Stmt_For {
                cond = cond,
                body = stmts,
            })
        case "let":
            l := p.token.loc
            parser_token_next(p)
            if ident, ok := parser_token_is(p, Identifier); ok {
                ident_loc := p.token.loc
                parser_token_next(p)
                if parser_op_match(p, .Assign) {
                    expr := parse_expr(p)
                    expect_1_skip_newlines(p)
                    return stmt_make(merge_locs(l, expr.loc), Stmt_Decl {
                        mutable = false,
                        name = ident,
                        value = expr,
                    })
                } else {
                    script_error(error_let_requires_initialization(p, merge_locs(l, ident_loc)))
                }
            } else {
                script_error(error_expected_token(p, Identifier, p.token))
            }
        case "var":
            l := p.token.loc
            parser_token_next(p)
            if ident, ok := parser_token_is(p, Identifier); ok {
                ident_loc := p.token.loc
                parser_token_next(p)
                if parser_op_match(p, Token_Operator.Assign) {
                    expr := parse_expr(p)
                    expect_1_skip_newlines(p)
                    return stmt_make(merge_locs(l, expr.loc), Stmt_Decl {
                        mutable = false,
                        name = ident,
                        value = expr,
                    })
                } else {
                    return stmt_make(l, Stmt_Decl {
                        mutable = false,
                        name = ident,
                        value = expr_make(merge_locs(l, ident_loc), Lit_Nil {}),
                    })
                }
            } else {
                script_error(error_expected_token(p, Identifier, p.token))
            }
        case "func":
            l := p.token.loc
            parser_token_next(p)
            is_cli := false
            if str, ok := parser_token_is(p, Lit_String); ok {
                if str.value != "cli" {
                    script_error(error_invalid_func_attribute(p, str.value))
                }
                is_cli = true
                parser_token_next(p)
            }
            ident := parser_token_expect(p, Identifier)
            lparen_token := p.token
            parser_op_expect(p, .LParen)
            skip_newline(p)
            ops := make([dynamic]Func_Param)
            parse_next_ok := true
            for !parser_op_match(p, .RParen) {
                if !parse_next_ok {
                    script_error(error_expected_token(p, Token_Operator.Comma, p.token))
                }
                if p.token.un == nil {
                    script_error(error_unterminated_token(p, lparen_token, Token_Operator.RParen))
                }
                name: Identifier
                loc := p.token.loc
                if ident, ok := p.token.un.(Identifier); ok {
                    name = ident
                } else {
                    script_error(error_expected_token(p, Identifier, p.token))
                }
                parser_token_next(p)
                append(&ops, Func_Param {
                    loc = loc,
                    name = name,
                })
                if !parser_op_match(p, .Comma) {
                    parse_next_ok = false
                }
            }
            l2 := p.token.loc
            body := parse_stmt_block(p)
            expect_1_skip_newlines(p)
            return stmt_make(merge_locs(l, l2), Stmt_Func {
                is_cli = is_cli,
                name = ident,
                body = body,
                params = ops[:],
            })
        case "return":
            loc := p.token.loc
            parser_token_next(p)
            expr := parse_expr(p)
            expect_1_skip_newlines(p)
            return stmt_make(merge_locs(loc, expr.loc), Stmt_Return {
                expr = expr,
            })
        case "break":
            loc := p.token.loc
            parser_token_next(p)
            return stmt_make(loc, Stmt_Break {})
        case "continue":
            loc := p.token.loc
            parser_token_next(p)
            return stmt_make(loc, Stmt_Continue {})
        }
    } else if parser_op_is(p, .LBrace) {
        return parse_stmt_block(p)
    }
    expr := parse_expr_toplevel(p)
    return stmt_make(expr.loc, Stmt_Expr { expr })
}

parse_stmt_block :: proc(p: ^Parser) -> ^Stmt {
    assert(parser_op_is(p, .LBrace))
    lbrace := p.token
    parser_token_next(p)
    skip_newline(p)
    stmts := make([dynamic]^Stmt)
    loc := p.token.loc
    for !parser_op_match(p, .RBrace) {
        if p.token.un == nil {
            script_error(error_unterminated_token(p, lbrace, .RBrace))
        }
        stmt := parse_stmt(p)
        for parser_op_is(p, .Ln) {
            parser_token_next(p)
        }
        append(&stmts, stmt)
    }
    loc = merge_locs(loc, p.token.loc)
    return stmt_make(loc, stmts[:])
}

parse_stmts :: proc(p: ^Parser) -> []^Stmt {
    stmts := make([dynamic]^Stmt)
    for {
        for parser_op_match(p, .Ln) {}
        if p.token.un == nil {
            break
        }
        stmt := parse_stmt(p)
        append(&stmts, stmt)
    }
    return stmts[:]
}
