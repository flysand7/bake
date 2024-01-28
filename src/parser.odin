package bake

import "core:unicode/utf8"
import "core:strconv"
import "core:strings"
import "core:intrinsics"

Unary_Op :: enum {

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
    Subscript,
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
    fn: Identifier,
    args: []^Expr,
}

Expr_Array :: struct {
    exprs: []^Expr,
}

Expr_Un :: union {
    Lit_String,
    Lit_Template,
    Lit_Int,
    Identifier,
    Expr_Unary,
    Expr_Binary,
    Expr_Ternary,
    Expr_Call,
    Expr_Array,
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
    text: string,
    idx: int,
    last_ch: rune,
    token: Token,
}

parser_make :: proc(text: string) -> Parser {
    p := Parser {
        text = text,
        idx = 0,
    }
    parser_char_next(&p)
    parser_token_next(&p)
    return p
}

parser_char_next :: proc(p: ^Parser) {
    if len(p.text[p.idx:]) == 0 {
        p.last_ch = 0
    }
    ch, sz := utf8.decode_rune_in_string(p.text[p.idx:])
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
        // TODO
    } else if parser_char_match(p, 'x') {
        // TODO
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
            offs = p.idx,
        },
    }
    if parser_char_is_range(p, 'a', 'z') || parser_char_is_range(p, 'A', 'Z') || parser_char_is(p, '_') {
        for parser_char_is_range(p, 'a', 'z') ||
            parser_char_is_range(p, 'A', 'Z') ||
            parser_char_is_range(p, '0', '9') ||
            parser_char_is(p, '_') ||
            parser_char_is(p, '-')
        {
            parser_char_next(p)
        }
        t.un = Identifier {
            name = p.text[t.loc.offs-1:p.idx-1],
        }
    } else if parser_char_is_range(p, '0', '9') {
        for parser_char_is_range(p, '0', '9') ||
            parser_char_is(p, '_')
        {
            parser_char_next(p)
        }
        num_str := p.text[t.loc.offs-1:p.idx-1]
        value, ok := strconv.parse_i64(num_str, 10)
        if !ok {
            panic("bad number")
        }
        t.un = Lit_Int {
            value = value,
        }
    } else if parser_char_is(p, '"') {
        str := strings.builder_make()
        parser_char_next(p)
        for !parser_char_is(p, '"') && !parser_char_is(p, 0) {
            if parser_char_is(p, '\\') {
                escaped := parser_match_string_escape(p)
                if escaped != 0 {
                    strings.write_encoded_rune(&str, escaped, false)
                } else {
                    panic("Bad string escape")
                }
            } else {
                strings.write_encoded_rune(&str, p.last_ch)
            }
            parser_char_next(p)
        }
        if parser_char_is(p, 0) {
            panic("Unterminated string literal")
        }
        ok := parser_char_match(p, '"')
        assert(ok)
        t.un = Lit_Template {
            value = strings.to_string(str),
        }
    } else if parser_char_is(p, '\'') {
        str := strings.builder_make()
        parser_char_next(p)
        for !parser_char_is(p, '\'') && !parser_char_is(p, 0) {
            if parser_char_is(p, '\\') {
                escaped := parser_match_string_escape(p)
                if escaped != 0 {
                    strings.write_rune(&str, escaped)
                } else {
                    panic("Bad string escape")
                }
            } else {
                strings.write_rune(&str, p.last_ch)
                parser_char_next(p)
            }
        }
        if parser_char_is(p, 0) {
            panic("Unterminated string literal")
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
        panic("Char doesn't start a token")
    }
    t.loc.width = p.idx - t.loc.offs
    p.token = t
}

parser_token_is :: proc(p: ^Parser, $T: typeid) -> (T, bool) {
    if v, ok := p.token.un.(T); ok {
        return v, true
    }
    return {}, false
}

parser_token_match :: proc(p: ^Parser, $T: typeid) -> bool {
    if p.token.un.(T) {
        parser_token_next(p)
        return true
    }
    return false
}

parser_op_is :: proc(p: ^Parser, op: Token_Operator) -> bool {
    if tok, ok := p.token.un.(Token_Operator); ok {
        if tok == op {
            return true
        }
    }
    return false
}

parser_op_match :: proc(p: ^Parser, op: Token_Operator) -> bool {
    if tok, ok := p.token.un.(Token_Operator); ok {
        if tok == op {
            parser_token_next(p)
            return true
        }
    }
    return false
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
    if parser_op_is(p, .Ln) {
        parser_token_next(p)
    }
}

expect_1_skip_newlines :: proc(p: ^Parser) {
    if !parser_op_is(p, .Ln) {
        panic("Expected newline")
    }
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
        if parser_op_match(p, .LParen) {
            skip_newline(p)
            args := make([dynamic]^Expr)
            loc := p.token.loc
            for !parser_op_is(p, .RParen) {
                if p.token.un == nil {
                    panic("Unterminated lparen")
                }
                append(&args, parse_expr(p))
                parser_op_match(p, .Comma)
                skip_newline(p)
            }
            loc = merge_locs(loc, p.token.loc)
            parser_op_match(p, .RParen)
            expr = expr_make(loc, Expr_Call {
                fn = ident,
                args = args[:],
            })
        } else if parser_op_match(p, .LBracket) {
            loc := p.token.loc
            rhs := parse_expr(p)
            loc = merge_locs(loc, p.token.loc)
            if !parser_op_match(p, .RBracket) {
                panic("Expected array subscript")
            }
            expr = expr_make(loc, Expr_Binary {
                op = .Subscript,
                lhs = expr_make(ident_loc, ident),
                rhs = rhs,
            })
        } else {
            expr = expr_make(ident_loc, ident)
        }
        return expr
    } else if parser_op_match(p, .LBracket) {
        skip_newline(p)
        exprs := make([dynamic]^Expr)
        loc := p.token.loc
        for !parser_op_is(p, .RBracket) {
            if p.token.un == nil {
                panic("Unterminated rbracket")
            }
            append(&exprs, parse_expr(p))
            parser_op_match(p, .Comma)
            skip_newline(p)
        }
        loc = merge_locs(loc, p.token.loc)
        parser_op_match(p, .RBracket)
        return expr_make(loc, Expr_Array{
            exprs = exprs[:],
        })
    } else if parser_op_match(p, .LParen) {
        skip_newline(p)
        defer parser_op_match(p, .RParen)
        return parse_expr(p)
    } else {
        panic("Expected an expression")
    }
    parser_token_next(p)
    return expr
}

parse_expr0 :: proc(p: ^Parser) -> ^Expr {
    lhs := parse_expr_simple(p)
    for parser_op_is(p, .Nvl) {
        t := p.token.un.(Token_Operator)
        parser_token_next(p)
        rhs := parse_expr_simple(p)
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
        if t == .Add {
            bop = .Add
        } else if t == .Sub {
            bop = .Sub
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
parse_expr :: proc(p: ^Parser) -> ^Expr {
    return parse_expr3(p)
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
                branch_t := parse_stmt_block(p)
                branch_f := cast(^Stmt) (nil)
                if ident, ok := parser_token_is(p, Identifier); ok && ident.name == "else" {
                    parser_token_next(p)
                    branch_f := parse_stmt_block(p)
                }
                expect_1_skip_newlines(p)
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
                    parser_token_next(p)
                    expr := cast(^Expr) nil
                    if parser_op_match(p, .Assign) {
                        expr = parse_expr(p)
                    }
                    expect_1_skip_newlines(p)
                    return stmt_make(merge_locs(l, expr.loc), Stmt_Decl {
                        mutable = false,
                        name = ident,
                        value = expr,
                    })
                } else {
                    panic("No ident")
                }
            case "var":
                l := p.token.loc
                parser_token_next(p)
                if ident, ok := parser_token_is(p, Identifier); ok {
                    parser_token_next(p)
                    expr := cast(^Expr) nil
                    if parser_op_match(p, Token_Operator.Assign) {
                        expr = parse_expr(p)
                    }
                    expect_1_skip_newlines(p)
                    return stmt_make(merge_locs(l, expr.loc), Stmt_Decl {
                        mutable = false,
                        name = ident,
                        value = expr,
                    })
                } else {
                    panic("No ident")
                }
            case "func":
                l := p.token.loc
                parser_token_next(p)
                if ident, ok := parser_token_is(p, Identifier); ok {
                    parser_token_next(p)
                    if !parser_op_match(p, .LParen) {
                        panic("expected '('")
                    }
                    ops := make([dynamic]Func_Param)
                    for !parser_op_match(p, .RParen) {
                        if p.token.un == nil {
                            panic("Unterminated '('")
                        }
                        name: Identifier
                        loc := p.token.loc
                        if ident, ok := p.token.un.(Identifier); ok {
                            name = ident
                        } else {
                            panic("ident expected")
                        }
                        parser_token_next(p)
                        append(&ops, Func_Param {
                            loc = loc,
                            name = name,
                        })
                        parser_op_match(p, .Comma)
                    }
                    l2 := p.token.loc
                    body := parse_stmt_block(p)
                    expect_1_skip_newlines(p)
                    return stmt_make(merge_locs(l, l2), Stmt_Func {
                        name = ident,
                        body = body,
                        params = ops[:],
                    })
                } else {
                    panic("No ident")
                }
        }
    }
    expr := parse_expr_toplevel(p)
    return stmt_make(expr.loc, Stmt_Expr { expr })
}

parse_stmt_block :: proc(p: ^Parser) -> ^Stmt {
    assert(parser_op_is(p, .LBrace))
    parser_token_next(p)
    skip_newline(p)
    stmts := make([dynamic]^Stmt)
    loc := Maybe(Loc) {}
    for !parser_op_match(p, .RBrace) {
        if p.token.un == nil {
            panic("Unterminated LBrace")
        }
        stmt := parse_stmt(p)
        for parser_op_match(p, .Ln) {}
        append(&stmts, stmt)
        if loc == nil {
            loc = stmt.loc
        } else {
            loc = merge_locs(loc.?, stmt.loc)
        }
    }
    return stmt_make(loc.?, stmts[:])
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
