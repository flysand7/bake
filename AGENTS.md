# Project instructions

Bake is a build system written in Odin. Instead of a DSL like make/ninja, it
runs imperative "bakefile" programs that emit build rules. The interpreter and
rule engine live in `src/`; the language spec is `doc/bake-language-reference.md`.

## Build and test

- Build the binary: `odin build src -out:bake.exe`
- Run unit tests: `odin test src`
- Smoke-test the language with the bakefiles under `testing/`:
  `testing/bakefile-syntax/bakefile`, `testing/funsies/`, `testing/hello-odin/`

Odin's test runner may report allocation leaks from the build/cache code while
still reporting all tests as successful. Treat a leak as a regression only when
it comes from code you changed.

Generated artifacts (`*.exe`, `*.pdb`, `*.rdi`, `.bake-cache`) are gitignored —
never hand-edit or commit them.

## Layout

- `src/bake.odin` — entry point (`main`)
- `src/bakefile.odin`, `src/parser.odin` — bakefile parsing
- `src/script.odin`, `src/script_builtin.odin`, `src/script_error.odin` — language runtime
- `src/build.odin`, `src/cache.odin`, `src/exec.odin` — rule evaluation, caching, command execution
- `src/file_windows.odin` — platform layer
- `src/cli/` — terminal/CLI helpers
- `doc/bake-language-reference.md` — language reference (source of truth)
- `testing/` — example and regression bakefiles

## Conventions

- Follow the surrounding Odin style; tests use `@(test)` procs in `*_test.odin`
  next to the code they cover.
- A language change is only complete when `parser.odin`, the script runtime, and
  `doc/bake-language-reference.md` are updated together.
- Add or extend a case in `testing/bakefile-syntax/bakefile` for parser/VM fixes.
- Keep OS-specific code isolated in `file_windows.odin`; don't sprinkle platform
  calls through shared code.

## Done means

- `odin build src -out:bake.exe` succeeds.
- `odin test src` passes.
- If the parser or VM changed, the relevant bakefiles in `testing/` still run.
