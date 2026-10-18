/*
    Command line interface utilities for bake.

    This provides a convenient interface to parse the command line arguments,
    parse its output, as well as make decisions based on the console on which
    bake runs. For example, this package figures out whether output coloring
    is needed by default, based on whether or not the terminal's output is
    redirected or not.

    This package is needed to avoid a lot of repeatable code when parsing
    command line arguments, as the underlying structure of the cli syntax is
    pretty much uniform.
    
    This package also performs some preliminary validation of the command line
    before returning the parsed arguments to the caller. The checks are done
    according to the schema, which the caller also must supply.

    ## Syntax

    The syntax for the bake's command-line is as follows:

        <exe> [args..] [opts..] [-- [targs..]]
    
    Where:

    - <exe>: The name of the executable (typically bake.exe or bake)
    - [args..]: List of positional arguments (not prefixed by '-')
    - [opts..]: List of options, which are prefixed by '-'
    - `--`: Separator between the bake's arguments and the arguments for target
    - [targs..]: List of arguments and options for the target.
    
    The entities in angled-brackets are non-optional. The entities in the square
    braces are optional.

    ## Options

    Options have a few possible syntaxes:

    - `-opt`: Boolean option. If not specified, defaults to `false`, otherwise `true`
    - `-opt:value`: String option. It is possible to explicitly set/reset booleans using this syntax.
*/
package bake_cli

import "core:os"
import "core:strings"

/*
    Specifies the type of error, related to a command-line argument.

    Error types that start with `Arg` relate to the argument as a whole. Error
    types that start with `Opt` relate to the option.
*/
Error_Kind :: enum {
    // Everything is okay!
    None,
    // Required positional argument is missing.
    Arg_Missing,
    // Unexpected positional argument.
    Arg_Unexpected,
    // Option is of the form `-:`.
    Opt_No_Name,
    // Option syntax contains `:`, but it's value is empty.
    Opt_No_Value,
    // Option with the specified name is not known.
    Opt_Unknown,
    // Given option was specified more than one time.
    Opt_Already_Specified,
    // Invalid value for option was specified.
    Opt_Invalid_Value,
}

/*
    Specifies possible command-line parsing errors.
*/
Error :: struct {
    // The type of error.
    kind: Error_Kind,
    // The argument the error is related to.
    arg: string,
    // If the error relates to an option (Except .Opt_No_Name errors), this
    // contains only the name of the option, without the value. E.g. for option
    // `-bakefile:foo` this contains just the `-bakefile` part.
    opt_name: string,
}

/*
    Configuration for the command-line interfacing.
*/
Config :: struct {
    stdout_color: bool,
    stderr_color: bool,
}

/*
    Type of option's values.
*/
Opt_Value :: union {
    // Boolean flag (e.g. `-verbose`)
    bool,
    // String (e.g. `-bakefile:<path>`).
    string,
}

/*
    Represents the parsed command-line
*/
Args :: struct {
    exe: string,
    args: []string,
    opts: map[string]Opt_Value,
    targs: []string,
}

Schema_Opt_Type :: enum {
    Boolean,
    String,
}

/*
    Option schema
*/
Opt_Schema :: struct {
    name: string,
    aliases: []string,
    type: Schema_Opt_Type,
}

/*
    Command-line parsing schema.
*/
Schema :: struct {
    opts: []Opt_Schema,
}

opt_schema :: proc(type: Schema_Opt_Type, name: string, aliases: []string = {}) -> Opt_Schema {
    return {
        name = name,
        aliases = aliases,
        type = type,
    }
}

find_opt_schema :: proc(schema: Schema, opt: string) -> (Opt_Schema, bool) {
    for s in schema.opts {
        if s.name == opt {
            return s, true
        }
        for a in s.aliases {
            if a == opt {
                return s, true
            }
        }
    }
    return {}, false
}

/*
    Global CLI configuration.
*/
config := Config {
    stdout_color = true,
    stderr_color = true,
}

/*
    Initializes the default settings for the cli package.
*/
init :: proc() {
    config.stdout_color = isatty(os.stdout)
    config.stderr_color = isatty(os.stderr)
}

/*
    Parse the command line into an appropriate data structure.
*/
parse_args :: proc(schema: Schema, os_args: []string, a := context.allocator) -> (Args, Error) {
    assert(len(os_args) > 0, "The `args` array must contain the executable's name.")
    exe := os_args[0]
    args := make([dynamic]string, a)
    opts := make(map[string]Opt_Value, a)
    targs_sep_idx := -1
    for arg, i in os_args[1:] {
        if arg[0] != '-' {
            append(&args, arg)
            continue
        }
        if arg == "--" {
            targs_sep_idx = i
            break
        }
        opt_sep_idx := strings.index_byte(arg, ':')
        if opt_sep_idx == -1 {
            opt_name := arg
            opt_schema, opt_schema_ok := find_opt_schema(schema, opt_name)
            if !opt_schema_ok {
                err := Error {
                    kind = .Opt_Unknown,
                    arg = arg,
                    opt_name = opt_name,
                }
                return {}, err
            }
            if opt_schema.type != .Boolean {
                err := Error {
                    kind = .Opt_No_Value,
                    arg = arg,
                    opt_name = opt_name,
                }
                return {}, err
            }
            if opt_schema.name in opts {
                err := Error {
                    kind = .Opt_Already_Specified,
                    arg = arg,
                    opt_name = opt_name,
                }
                return {}, err
            }
            opts[opt_schema.name] = true
            continue
        }
        opt_name := arg[:opt_sep_idx]
        if len(opt_name) == 0 {
            err := Error {
                kind = .Opt_No_Name,
                arg = arg,
            }
            return {}, err
        }
        if opt_sep_idx+1 == len(arg) {
            err := Error {
                kind = .Opt_No_Value,
                arg = arg,
                opt_name = opt_name,
            }
            return {}, err
        }
        opt_schema, opt_schema_ok := find_opt_schema(schema, opt_name)
        if !opt_schema_ok {
            err := Error {
                kind = .Opt_Unknown,
                arg = arg,
                opt_name = opt_name,
            }
            return {}, err
        }
        if opt_schema.name in opts {
            err := Error {
                kind = .Opt_Already_Specified,
                arg = arg,
                opt_name = opt_name,
            }
            return {}, err
        }
        if opt_schema.type == .String {
            opts[opt_schema.name] = arg[opt_sep_idx+1:]
            continue
        }
        opt_value: Opt_Value = ---
        opt_expect_type: Schema_Opt_Type = ---
        switch arg[opt_sep_idx+1:] {
        case "true": opt_value = true
        case "false": opt_value = false
        case:
            err := Error {
                kind = .Opt_Invalid_Value,
                arg = arg,
                opt_name = opt_name,
            }
            return {}, err
        }
        opts[opt_schema.name] = arg[opt_sep_idx+1:]
    }
    for s in schema.opts {
        if s.name not_in opts {
            if s.type == .Boolean {
                opts[s.name] = false
            }
        }
    }
    if(targs_sep_idx == -1) {
        parsed_args := Args {
            exe = exe,
            args = args[:],
            opts = opts,
            targs = nil,
        }
        return parsed_args, {}
    }
    targs := make([dynamic]string, a)
    for arg in os_args[targs_sep_idx+1:] {
        append(&targs, arg)
    }
    parsed_args := Args {
        exe = exe,
        args = args[:],
        opts = opts,
        targs = targs[:],
    }
    return parsed_args, {}
}
