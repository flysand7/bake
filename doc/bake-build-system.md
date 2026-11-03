# Bake build system reference

Bake turns a bakefile into a dependency graph and runs the commands needed to
produce the requested files. This document covers recipes (`bake-recipe`), build
requests (`bake-build`), how the graph is ordered, when a recipe is considered
out of date, how the cache works, and how commands are executed.

The language used to write bakefiles is covered separately in the
[bake language reference](bake-language-reference.md).

> **Status.** The build engine is an early proof of concept. Several behaviors
> described below are surprising and are expected to change. The
> [current limitations](#current-limitations) section collects the rough edges;
> where a behavior is the result of a known implementation bug, this document
> says so explicitly.

## Running bake

```
bake [options] [target] -- [args]
```

* The target is the name of a function in the bakefile. It defaults to `build`,
  which must be declared as `func 'cli' build(args)`.
* Arguments after `--` are passed to the target function as `args`.
* If `-bakefile:<path>` is not given, bake starts in the current working
  directory and walks up through parent directories looking for a file named
  `bakefile`.
* Other options: `-cache:<path>`, `-verbose` (or `-v`), `-colors`, `-help`.

Only one target function runs per invocation. That function usually ends with
one or more `bake-build` calls.

## Recipes

```bake
bake-recipe(cmds, inputs, outputs)
```

| Argument | Meaning |
| --- | --- |
| `cmds` | Array of commands. Each command is an array of strings: the program followed by its arguments. |
| `inputs` | Array of paths the recipe reads. |
| `outputs` | Array of paths the recipe produces. |

All three arguments must be arrays. Every element of a command and every path is
converted to a string with the language's string conversion, so numbers work;
`nil` becomes the empty string.

Calling `bake-recipe` does not run anything. It appends a recipe to a global
list. Recipes accumulate for the rest of the script run and are candidates for
every later `bake-build` call.

### Commands

A command is an argument vector, not a command line. Bake starts the program
directly:

* no shell is involved: pipes, redirection (`>`), `&&`, globs, `%VAR%`/`$VAR`
  expansion, and shell quoting rules do not apply;
* if shell features are needed, invoke a shell explicitly, for example
  `['cmd', '/c', 'type a.txt > b.txt']` on Windows;
* each command is a separate process; its standard output and standard error
  are redirected to the null device and discarded, and its standard input is
  also the null device. Bake does not forward or capture program output. To see
  a tool's output, make the command write it to a file (for example, run a shell
  with redirection);
* the exit code is checked after each command.

The commands inside one recipe run in the order listed. A command must be an
array; passing a single string instead is an error.

### Inputs and outputs

* Paths are opaque strings used verbatim: there are no globs, no path
  normalization, and no case folding. `./main.c` and `main.c` are different
  strings.
* Outputs define what a recipe can produce. Targets and dependency lookups match
  these strings exactly.
* Inputs are classified automatically when the graph is built:
  * **intermediate dependency** -- an input string equal to some recipe's output;
    that recipe becomes a dependency;
  * **leaf dependency** -- every other input, assumed to exist in the project.

### Unreachable recipes

A recipe with no outputs can never be selected as a target and can never satisfy
another recipe's input, so it never runs.

If two recipes list the same output, only the first one registered can ever be
selected: lookup returns the first recipe whose outputs contain the requested
string.

## Requesting a build: bake-build

```bake
bake-build(targets)
```

`targets` is an array of paths. The call builds only what is needed for those
targets; recipes that are not reachable from them are ignored.

* Every target must be produced by a recipe registered before the call.
  Otherwise bake panics with `No recipe produces a desired target` and the
  process aborts. The cache file is not written for that call, but any commands
  run by earlier `bake-build` calls have already happened.
* Recipes registered after a call cannot affect that call, but they are visible
  to later calls.
* Each call independently opens the cache, builds a plan, executes it, and
  writes the cache. This means a later call sees the cache updates made by an
  earlier call in the same script run.

Example with two calls:

```bake
func 'cli' build(args) {
    bake-recipe([['cmd', '/c', 'type gen.txt > mid.txt']], ['gen.txt'], ['mid.txt'])
    bake-build(['mid.txt'])

    bake-recipe([['cmd', '/c', 'type mid.txt > final.txt']], ['mid.txt'], ['final.txt'])
    bake-build(['final.txt'])
}
```

Step by step on a clean directory:

1. The script registers the `mid.txt` recipe, then the first `bake-build` runs.
   `mid.txt` is missing, so its recipe executes.
2. The script registers the `final.txt` recipe. The second `bake-build` replans
   from `final.txt`; the `mid.txt` recipe is discovered as a dependency, but
   `mid.txt` is now cached and unchanged, so only the `final.txt` recipe runs.
3. On a second invocation of bake, both calls find everything unchanged and run
   nothing.

## Building the dependency graph

For each target, in the order given:

1. Find the first registered recipe that lists the target as an output.
2. For each input of that recipe, recursively apply the same lookup. Inputs with
   no producing recipe become leaves.
3. Emit the recipe after all of its dependencies have been emitted.

The walk is a depth-first post-order traversal, performed separately for each
target. Its practical consequences:

### Ordering

A recipe is always emitted after its dependencies, so a dependency executes
first. Across independent branches, order follows the target order and the order
of inputs inside each recipe. It is not a stable global topological order.

### Cycles are not detected

If a recipe's input is produced by a recipe that directly or indirectly consumes
its output, the recursive walk never terminates. Bake crashes with a stack
overflow (exit code `0xC00000FD` on Windows) without a diagnostic. Keep the graph
acyclic.

## Deciding what to rebuild

After the graph is sorted, bake builds the complete execution plan **before
running anything**. For each node in plan order:

1. Check every input.
   * If any input's status is not `Unchanged`, mark the recipe for execution and
     skip to step 4.
2. Otherwise check every output.
   * For each output with status `Updated`, print
     `[WARNING]: Output <path> unexpectedly changed since last rebuild. Rebuilding.`
   * If any output's status is not `Unchanged`, mark the recipe for execution.
3. If neither inputs nor outputs require a rebuild, skip the recipe. With
   `-verbose` this is reported as `* Cached!`.
4. Record the decision and continue with the next node.

A node's decision is made from the cache as it was at the start of the
`bake-build` call.

### Caching behavior

The cache stores one record per file. A record contains the file's mtime
(modification time), it's size and the content hash. Checking whether any of the
inputs were changed involves first checking the `(mtime, size)` of the file and
if the pair is unchanged, the file is considered unchanged. Otherwise the hash
is recomputed and checked against the cached version.

> [!WARNING] Tools lying about time
>
> Some tools (for example 7z) might change the file's mtime after extraction,
> causing the file to have mtime that is earlier than the time of the last
> change. If such tools are used, there is a possibilty that the file contents
> change without updating the `(mtime, size)` pair, causing the stale cache.
>
> It is advised to be aware of these tools and either delete the cache file in
> order to free it from staleness, or use alternative tools.

`cache_check` works like this:

1. Open the file. If it cannot be opened, the status is `Not_Exist`.
2. Find the record whose identity matches the file.
3. If the file's mtime or the size differs from the cached record, then the
   file status is `Updated`.
4. Otherwise the file's contents are hashed. If the hash differs, the file status
   is `Updated`.
5. Otherwise the status is `Unchanged`.

The implications of this are the following: Unless tools lie about file's mtime,
doing a `touch` of the file should not trigger a re-build, because even if the
mtime has changed, the file contents have not due to a source hash check - it's
still the same file.

### Missing files

* A missing leaf input has status `Not_Exist`, which counts as a change, so the
  recipe is scheduled on every run. If the command tolerates the missing file,
  the recipe rebuilds every time; if it does not, it fails every time. Bake does
  not validate leaf inputs up front.
* A missing output also has status `Not_Exist` and triggers a rebuild, so
  deleting an output forces its recipe to run again. Bake does not verify that a
  successful command actually created its outputs; a recipe that silently
  produces nothing is simply rebuilt next time.

## The cache file

Bake keeps the cache in a file called `.bake-cache` in the working directory.
The location of this file can be changed by `-cache:<path>` command-line option.

### Format

The cache file is a binary file, that has a magic number (string `'bake'` in
ASCII), version information, header information and an array of file information
records.

### Lifecycle

If the cache file is missing, it is automatically created by `bake`, and treats
every file as *changed*. This is convenient for doing uncached rebuilds, just
delete the file and re-run bake.

This file is saved to disk after every `bake-build` call. This includes calls
within the same `bake` invokation. It stores information necessary to know,
whether or not any files have changed since last call to `bake-build`.

Records are never pruned from the cache. When a cached file is deleted,
information about this file still remains in the cache. This is important, as
building one module using `bake-build` should not invalidate the cache for
another module.

## Executing the plan

The plan runs sequentially, in plan order. For each recipe:

1. Run its commands in order.
2. After the recipe -- successful or not -- record its outputs and inputs in the
   cache.

### Working directory

Commands run in the working directory of the `bake` process. Recipe paths are
resolved by the commands themselves and by the file-identity lookup relative to
that directory.

`-bakefile:<path>` selects the script but does **not** change the working
directory. Outputs land where bake was started, not next to the bakefile. This
was verified by running `bake -bakefile:<temp>/bakefile` from the repository
root: the recipe outputs were created in the repository root.

Practical rule: run bake from the project root and write recipe paths relative
to it. If a command needs another directory, have it change directory itself
(for example `['cmd', '/c', 'cd /d C:/project && ...']`); bake has no per-recipe
working directory.

### Failures

Actual behavior:

* If a command exits with a non-zero code, bake prints
  `Command <argv> failed with error code: <n>` and skips the remaining commands
  of that recipe. The failing command's own diagnostics are not shown, because
  child output is discarded.
* Execution continues with the next recipe in the plan, including recipes that
  depend on the failed one.
* The failed recipe's outputs and inputs are recorded in the cache anyway.
* At the end, bake exits with status 0. There is no failure exit code and no
  summary.

Walkthrough. Recipes: `s1.txt` from the missing `missing-input.txt`, `s2.txt`
from `s1.txt`, `s3.txt` from `s2.txt` (its command is `exit /b 5`), and
`indep.txt` with no inputs. Request `s3.txt` and `indep.txt`:

1. The `s1.txt` recipe runs `type missing-input.txt > s1.txt`; it fails with
   code 1. The shell redirection still creates an empty `s1.txt`, and that file
   is cached.
2. The `s2.txt` recipe runs and copies the empty `s1.txt`.
3. The `s3.txt` recipe runs `exit /b 5`, fails with code 5, and produces no
   output. The cache still records an entry for the missing `s3.txt` (a zeroed
   record, which can never match because the file cannot be opened).
4. The `indep.txt` recipe runs.
5. The process exits with status 0.

On the next run, the `s1.txt` recipe is scheduled again (its input is still
missing), the `s2.txt` recipe is cached, the `s3.txt` recipe is scheduled again
(its output is still missing), and `indep.txt` is cached. The build never
reports a failure status, even though it keeps failing.

If the executable cannot be started at all, the command is reported as failing
with code 127. Other process-start errors panic.

## Worked example: compiling a C program

Assume the working directory contains `main.c`, `util.c`, and `parser.c`, and
bake is started there.

```bake
func 'cli' build(args) {
    let sources = ['main.c', 'util.c', 'parser.c']

    var i = 0
    for i < len(sources) {
        let src = sources[i]
        let obj = src + '.o'
        bake-recipe([['clang', '-c', src, '-o', obj]], [src], [obj])
        i = i + 1
    }

    bake-recipe(
        [['clang', 'main.c.o', 'util.c.o', 'parser.c.o', '-o', 'app.exe']],
        ['main.c.o', 'util.c.o', 'parser.c.o'],
        ['app.exe'],
    )
    bake-build(['app.exe'])
}
```

First run, step by step:

1. The script executes. Three object recipes are appended, then the link recipe.
2. `bake-build(['app.exe'])` resolves `app.exe` to the link recipe.
3. The link recipe's inputs are outputs of the object recipes, so each object
   recipe becomes a dependency node. Their `*.c` inputs are leaves.
4. The plan order is: `main.c.o`, `util.c.o`, `parser.c.o`, `app.exe`.
5. Every output is missing, so every recipe is scheduled.
6. The commands run in that order, and the cache records the sources and
   outputs.
7. The cache file is written.

Second run with no changes: every input and output is `Unchanged`, all four
recipes report `Cached!`, and no commands run.

Now `util.c` is modified:

1. Planning: `util.c` is `Updated`, so the `util.c.o` recipe is scheduled.
2. Planning: `app.exe` is `Unchanged`, so the link recipe is **not** scheduled;
   the planner does not know that `util.c.o` is about to be rewritten.
3. Execution: `util.c.o` is rebuilt, but `app.exe` keeps the old object's code.
4. Later runs report everything as `Unchanged`, so `app.exe` stays stale.

To get the intended rebuild with the current planner, list the sources in the
link recipe's inputs as well:

```bake
        ['main.c.o', 'util.c.o', 'parser.c.o', 'main.c', 'util.c', 'parser.c'],
```

Then modifying `util.c` makes both the `util.c.o` recipe and the link recipe
dirty during planning, and the link runs after the object has been rebuilt in
the same plan.

## Debugging the planner with -verbose

`-verbose` (or `-v`) prints the plan and the execution decisions. The lines to
look for:

| Line | Meaning |
| --- | --- |
| `ADD BAKE-RECIPE: <cmds>` | A recipe was registered. |
| `RUN BAKE-BUILD...` | A `bake-build` call started. |
| `Building task dependency graph...`, `Sorting dependency graph...`, `Building execution plan...` | Phases of the call. |
| `Considering graph node: <cmds>` | The planner is looking at one recipe. Duplicated recipes appear multiple times. |
| `* File '<path>' status: <status>` | Cache status of an input or output. |
| `* Input(s) changed.` / `* Input(s) are unchanged.` | Result of the input checks. |
| `* Output(s) changed.` / `* Output(s) are unchanged.` | Result of the output checks; only reached when the inputs are unchanged. |
| `* This recipe will be executed.` | The recipe was added to the plan. |
| `* Cached!` | The recipe was skipped. |
| `RUN COMMAND: <argv>` | A command is about to run. |
| `* Cached file: '<path>'` | A record was written after a recipe ran. |

Read a trace from top to bottom: status lines belong to the `Considering...`
line above them, and all planning lines appear before the first `RUN COMMAND`
line. A `Cached!` printed before any command is a planning decision, not a
report about a command.

## Current limitations

Known shortcomings of the current implementation, beyond the behaviors described
above:

* **No deduplication** -- shared dependencies and repeated targets run once per
  path.
* **No cycle detection** -- a cyclic graph crashes with a stack overflow.
* **Failures do not fail the build** -- execution continues, the exit status is
  0, and failed recipes are cached.
* **Timestamp-only freshness** -- no content hashing; older or equal timestamps
  are invisible.
* **File identity reuse** -- records are keyed by NTFS file index, which can be
  reused after deletion.
* **Windows-only** -- the file identity layer is implemented only for Windows;
  there is no implementation for other platforms.
* **No globs or path normalization** -- inputs, outputs, and targets are matched
  as exact strings.
* **No script or environment tracking** -- editing the bakefile, changing
  environment variables, or changing tool versions does not trigger a rebuild
  unless an input file's timestamp changes.
* **Command output is discarded** -- child standard output and error are
  redirected to the null device, so compiler diagnostics and program output are
  invisible; only bake's own messages are printed.
* **No concurrency** -- the plan runs sequentially.
* **Unpruned cache** -- records for deleted or renamed files are never removed,
  and missing files are recorded under a zeroed identity.
* **No per-recipe working directory** -- all commands share the process working
  directory.

## See also

* [Bake language reference](bake-language-reference.md)
* `bake -help` for command-line options.
