# Project instructions

Bake is a build system written in Odin. Instead of a DSL like make/ninja, it
runs imperative "bakefile" programs that emit build rules. The interpreter and
rule engine live in `src/`; the documentation can be found in the `doc/`
directory.

## Build and test

- Build the binary: `odin build src -out:bake.exe`
- Run unit tests: `odin test <package>`, where `<package>` is a directory
  containing `*_test.odin` files. Only the tests from that directory will be
  ran. The tests from sub-directories won't be executed automatically.
- Smoke-test the language with the bakefiles under `testing/`:
  `testing/bakefile-syntax/bakefile`, `testing/funsies/`, `testing/hello-odin/`

Odin's test runner may report allocation leaks from the build/cache code while
still reporting all tests as successful. Treat a leak as a regression only when
it comes from code you changed.

Generated artifacts (`*.exe`, `*.pdb`, `*.rdi`, `.bake-cache`) are gitignored —
never hand-edit them. When working on a change, do not use `git add .` or
`git commit`.

## Layout

- `src/bake.odin` — entry point (`main`)
- `src/bakefile.odin`, `src/parser.odin` — bakefile parsing
- `src/script*.odin` — language runtime
- `src/build.odin` - rule evaluation engine
- `src/exec.odin` — command execution
- `src/cache/` - file info caching
- `src/cli/` — terminal/CLI helpers
- `doc/` — documentation (source of truth)
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

- `odin vet` passes.
- `odin build src -out:bake.exe` succeeds.
- `odin test src` passes.
- Bake can build itself using `bake build`
- If the parser or VM changed, the relevant bakefiles in `testing/` still run.
