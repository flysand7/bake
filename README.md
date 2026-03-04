
# Bake

Bake is a build system that uses an imperative programming language to produce
build rules.

The traditional build systems (e.g. make, ninja) use a DSL to specify rules in
the recipe file. A separate script, written in Python or Lua could then be used
to generate the recipe file.

## Getting started

In order to try out bake, clone this repository onto your machine. Make sure
that you have Odin. Then run the following command:

```sh
odin build src -out:bake.exe
```
