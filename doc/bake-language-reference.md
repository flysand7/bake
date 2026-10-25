# Bake language reference

Bake is the dynamically typed, imperative language used to write `bakefile`
scripts. A script executes from top to bottom and emits build recipes; this
document covers the language itself, not the command-line interface or the
dependency graph. The build system -- recipes, targets, the dependency graph, and
the cache -- is documented in
[bake-build-system.md](bake-build-system.md).

For a runnable tour of the syntax, see
[`testing/bakefile-syntax/bakefile`](../testing/bakefile-syntax/bakefile).

## Lexical structure

| Element | Rules |
| --- | --- |
| Comments | `#` to the end of the line |
| Identifiers | Start with an ASCII letter or `_`; then letters, digits, `_`, `-` |
| Integers | Decimal, 64-bit signed; `_` may separate digits |
| Strings | Single quotes, e.g. `'main.c'` |
| Templates | Double quotes, e.g. `"out/${name}.o"` |
| Literals | `nil`, `true` (equal to `1`), `false` (equal to `0`) |

Hyphens are valid in identifiers, so `a-b` is a single name. This is why
subtraction is written with `->` instead of `-`.

Integers have no sign, radix prefix, or fractional part: `0`, `42`, and
`1_000_000` are valid; `-1`, `0x10`, and `1.5` are not.

### Escapes

Both string forms accept the same escape sequences:

| Escape | Meaning |
| --- | --- |
| `\a` | Alert (bell) |
| `\b` | Backspace |
| `\e` | Escape, `0x1B` |
| `\n` | Line feed |
| `\r` | Carriage return |
| `\t` | Tab |
| `\v` | Vertical tab |
| `\uXXXX` | Unicode code point from four hex digits |
| `\xXX` | Byte from two hex digits |

A backslash always starts an escape sequence, so prefer `/` in paths:
`'C:/Users/Bob'`.

### Template strings

`${name}` is replaced with the value of the variable `name`:

```
let target = 'main'
print("building ${target}\n")
```

Only a variable lookup is supported inside `${...}`; expressions are not
evaluated.

## Values

| Type | Description |
| --- | --- |
| `nil` | Absence of a value |
| int | Signed 64-bit integer |
| string | UTF-8 byte sequence |
| array | Ordered, heterogeneous, zero-based |
| dict | Map with string keys and heterogeneous values |
| function | User-defined or built-in |

There is no separate boolean type. `true` and `false` are the integers `1` and
`0`. The falsy values are `nil`, `0`, `''`, `[]`, and `{}`; every other value,
including functions, is truthy.

Arrays and dictionaries are reference values: assigning or passing one shares
its contents. `+` on arrays creates a new array. Integer arithmetic is 64-bit
two's complement; overflow wraps around.

### Collections

Array literals use square brackets, may hold any mix of values, and allow a
trailing comma:

```
let sources = ['main.c', 'util.c']
let mixed = [1, 'two', nil, [3]]
```

Dictionary literals use curly braces with identifier keys:

```
let config = {
    name = 'app',
    sources = ['main.c'],
}
```

Keys in a literal must be identifiers; quoted keys are not accepted there. Read
them as `config.name` or `config['name']`. `[]` is an empty array and `{}` is an
empty dictionary.

## Expressions

Expressions are listed from lowest to highest precedence:

| Precedence | Operators | Associativity |
| --- | --- | --- |
| 1 | `implies` | left |
| 2 | `or`, `xor` | left |
| 3 | `and` | left |
| 4 | `==`, `!=`, `<`, `<=`, `>`, `>=` | left |
| 5 | `+`, `->` | left |
| 6 | `*`, `/` | left |
| 7 | `??` | left |
| 8 | `!` | prefix |
| 9 | `.name`, `(args)`, `[index]` | left |
| 10 | literals, names, `(expr)`, arrays, dicts | — |

`??` binds tighter than `*`, so `6 / nil??2` is `6 / (nil ?? 2)` and evaluates
to `3`.

### Arithmetic and concatenation

| Operator | Operands | Result |
| --- | --- | --- |
| `+` | int, int | Sum |
| `+` | array, array | New concatenated array |
| `+` | other | Both sides converted to strings and concatenated |
| `->` | int, int | **Right operand minus left operand** |
| `*` | int, int | Product |
| `/` | int, int | Integer division, truncated toward zero; division by zero is an error |
| `/` | string, string | Strings joined with `/` |

`5 -> 2` evaluates to `-3`. There is no bare `-` operator.

`*`, `->`, and integer `/` accept only integers. Functions are rejected by
every binary operator.

### Logic

`!` negates truthiness. `and`, `or`, `xor`, and `implies` combine truthiness
values and produce `1` or `0`. The binary logical operators do not short-circuit:
both operands are always evaluated.

| `a` | `b` | `a and b` | `a or b` | `a xor b` | `a implies b` |
| --- | --- | --- | --- | --- | --- |
| 0 | 0 | 0 | 0 | 0 | 1 |
| 0 | 1 | 0 | 1 | 1 | 0 |
| 1 | 0 | 0 | 1 | 1 | 1 |
| 1 | 1 | 1 | 1 | 0 | 1 |

In other words, `a implies b` is `a or not b`.

`a ?? b` returns `a` unless it is `nil`, in which case it returns `b`. Both
operands are evaluated.

### Comparison

`==` and `!=`:

- If either side is `nil`, they compare nilness.
- Integers compare numerically.
- Arrays compare element-wise, recursively.
- If either side is a string, both sides are converted to strings and compared.
- Any other combination, including two dictionaries, raises an error.

`<`, `<=`, `>`, and `>=`:

- If either side is `nil`, the result is `0` (false).
- Integers compare numerically.
- If either side is a string, both sides are converted to strings and compared
  lexicographically.
- Other combinations raise an error.

Because string conversion applies when either side is a string, `1 == '1'` is
true.

### Access and calls

| Form | Meaning |
| --- | --- |
| `dict.name` | Member value; a missing key is an error |
| `dict['name']` | Key value; a missing key returns `nil` |
| `array[index]` | Zero-based element; out-of-bounds is an error |
| `callable(args)` | Call with exactly the declared number of arguments |

Any expression that evaluates to a function can be called, including members
such as `counter.inc()`. Arguments are evaluated left to right. Strings cannot
be indexed.

### Assignment

Assignment is a statement, not a general expression; it cannot be nested inside
another expression.

```
x = 1
dict.key = 2
array[0] = 3
```

- `x = value` updates the nearest visible binding of `x`. If no binding exists,
  a new variable is created in the current scope.
- `dict.key = value` and `array[index] = value` mutate the container in place.
- There are no compound assignment operators such as `+=`.

## Statements

| Form | Meaning |
| --- | --- |
| `let name = expr` | Declare a variable |
| `var name = expr` | Declare a mutable variable |
| `name = expr` | Assign to a variable |
| `expr` | Evaluate and discard |
| `{ ... }` | Block, introduces a scope |
| `if cond stmt` … `else stmt` | Conditional |
| `for cond { ... }` | Loop while the condition is truthy |
| `break`, `continue` | Loop control |
| `return expr` | Return from a function |
| `func name(params) { ... }` | Declare a function |
| `func 'cli' name(params) { ... }` | Declare a command-line target |

The initializer is optional and defaults to `nil`. A name may not be declared
twice in the same scope, and built-in names (`print`, `len`, `cmd`, and so on)
may never be declared. Nested scopes may shadow variables from outer scopes.

> [!NOTE]
> `let` is intended for immutable bindings and `var` for mutable ones, but the
> interpreter does not enforce mutability yet, so both can be reassigned.

Conditions use truthiness. The body of `if` is a single statement, while the
body of `for` must be a block. `else` is optional, and `else if` works because
the else branch is itself a statement. `continue` skips the rest of the current
iteration; `break` exits the loop.

### Line rules

Newlines matter in a few places:

| Construct | Rule |
| --- | --- |
| `let` / `var` | Newline required after the declaration |
| `return expr` | Newline required after the expression |
| `for` and `func` bodies | Newline required after the closing `}` |
| `if` body | Must start on the same line as the condition |
| `else` | Must be on the same line as the `}` of the true branch |
| Inside `[]`, dictionary literals, and call arguments | Newlines are ignored |
| After binary operators | Newlines are ignored |

Statements normally occupy separate lines. There are no semicolons.

### Scope

The top level is the global scope. Every block creates a child scope, including
function bodies and each iteration of a loop body. Assignment searches outward
through enclosing scopes; declarations always belong to the current one.

## Functions

```
func add(a, b) {
    return a + b
}
```

- Functions are first-class: they can be stored in variables, arrays, and
  dictionaries, passed as arguments, and returned.
- Functions are closures: they capture the scope in which they were declared.
- Arity is exact; there are no default, optional, or variadic parameters.
- `return` requires an expression. A function that ends without `return`
  produces `nil`.
- A function declaration takes effect when execution reaches it; using the name
  before that point is an error.
- Functions may recurse, because their name is bound in the enclosing scope.

The optional `'cli'` attribute marks a function as a command-line target:

```
func 'cli' build(args) {
    print('target called with ', len(args), ' arguments\n')
}
```

A target receives one argument: an array of strings.

## Built-ins

| Name | Signature | Description |
| --- | --- | --- |
| `print` | `print(...)` | Concatenates the string form of each argument; no separator or newline; returns `nil` |
| `len` | `len(value)` | Byte length of a string or element count of an array; other types abort |
| `cmd` | `cmd(argv)` | Runs an array of command-line strings; returns the exit code, or `127` if the executable is not found |
| `bake-recipe` | `bake-recipe(cmds, inputs, outputs)` | Registers a recipe; `cmds` is an array of argument arrays |
| `bake-build` | `bake-build(targets)` | Builds the requested outputs from registered recipes using the cache |
| `assert` | `assert(condition, message?)` | Exits the process with status 1 if the condition is falsy |

```
bake-recipe(
    [['cc', '-c', 'main.c', '-o', 'main.o']],
    ['main.c'],
    ['main.o'],
)
bake-build(['main.o'])
```

`assert` is an intrinsic special form: it is recognized only as a direct call
to the name `assert` and cannot be stored or passed as a value.

### String conversion

`print`, template interpolation, and string concatenation use the same
conversion:

| Value | Text |
| --- | --- |
| `nil` | Empty string |
| int | Decimal representation |
| string | The string itself |
| array | `[1, 2]`; strings inside are quoted, as in `['a', 1]` |
| dict | `[key=value, ...]`; ordering is unspecified |
| function | Its name, or `<built-in@address>` for built-ins |

## Errors

Bake reports errors with a category letter, a numeric code, and a source
location:

```
Error[L0001]@bakefile(1,13): Invalid escape sequence '\x'
Error[P0007]@bakefile(3,24): Expected token 'newline', instead got ']'
Error[R0013]@bakefile(4,1): Identifier 'else' was not declared
```

- `L` errors are lexical, `P` errors are syntactic, and `R` errors are runtime
  errors.
- Lexical and syntax errors abort before any statement executes.
- Runtime errors abort at the failing statement.
- `assert` reports `Assertion failed(line, col): message` and exits with
  status 1.

## Not in the language

For completeness, Bake has no: floating-point, hexadecimal, binary, or octal
literals; negative literals; unary minus; a bare `-` operator; modulo (`%`);
bitwise operators; `&&` / `||`; a ternary `?:`; compound assignment (`+=`, `-=`,
and so on); `++` / `--`; `while`; `for ... in`; `switch`; exceptions; imports or
modules; user-defined types; string indexing; short-circuit logical operators;
default or variadic parameters; semicolons; or block comments.
