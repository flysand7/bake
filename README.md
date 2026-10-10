
# Bake

> [!CAUTION]
> Bake is currently a proof-of-concept project. The current state of the project
> is far from usable and contains many bugs. Do not use it in production
> systems.

Bake is a build system that uses an imperative programming language to produce
build rules.

The traditional build systems (e.g. make, ninja) use a DSL to specify rules in
the recipe file. The big problem that bake tries to solve is that DSL is
fundamentally not cross-platform and depends on the OS shell to execute
commands. To circumvent this, programmers can write a separate script, written
in Python or Lua could then be used to generate the recipe file.

Bake is an attempt to unify these two steps into a single step. It allows you
to write procedural, cross-platform scripts that generate build ruleset that
will compare against the previous execution and run only those steps that really
need to be performed based on changed files.

## Getting started

In order to try out bake, clone this repository onto your machine. Make sure
that you have Odin. Then run the following command:

```sh
odin build src -out:bake.exe
```
