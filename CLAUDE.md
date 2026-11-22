# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Interpreter for **MAO** (*Modello Astratto Operazionale*), a small imperative teaching language of the 1st-year *Programmazione & Algoritmica* course. The OCaml core runs from the CLI, and it also runs in the browser through `wasm_of_ocaml` as a playground with a step-by-step execution trace, deployed to GitHub Pages.

- The language reference is the course notes in `reference.pdf` (intentionally left untracked). Test names cite its sections and rules, e.g. `(4.4)` or `(E-Bop)`. Deviations from the notes are deliberate and labelled in tests, e.g. short-circuit `&&`/`||`.
- The users are students. **Every user-facing string is in Italian**: error messages, trace text and the web UI. Code identifiers and test names are in English.
- Planned work is in the README: closures, syntactic sugar (`else if`, `for`) and keeping comments. The lexer currently drops comments, so the formatter loses them; the web UI warns before formatting a program that has comments.

## Commands

The project has a local opam switch in `_opam/`. Run tools through `opam exec --` unless the shell has already run `eval $(opam env)`.

```bash
opam switch create . --deps-only --with-dev-setup   # first-time setup (OCaml >= 5.4)
opam exec -- dune build                             # build everything
opam exec -- dune build @runtest                    # all tests (what CI runs)
opam exec -- dune exec test/test.exe -- test Interpreter  # a single alcotest group
opam exec -- dune fmt                               # ocamlformat 0.29.0, applies changes in place
cat sample/fib.mao | opam exec -- dune exec mao_run # run a program, print its trace
cat sample/fib.mao | opam exec -- dune exec mao_fmt # pretty-print a program
opam exec -- ./scripts/build_site.sh                # build the web site into _site/
python3 -m http.server -d _site 8000                # serve it locally
```

CI (`.github/workflows/deploy.yml`) runs the tests and `build_site.sh` on every push to `main`, then deploys `_site` to GitHub Pages.

## Architecture

The pipeline is lexer → parser → type checker → interpreter → trace. `lib/maoLang/maoLang.ml` connects the stages, re-exports them under short names (`MaoLang.Parser`, `MaoLang.Interpreter`, …) and adds the Italian error prefixes (`Errore lessicale`, `Errore di sintassi`, `Errore di tipo`, `Errore di esecuzione`).

- **Two ASTs** (`maoAst.mli`, a type-only module). The parser produces `command`/`expr`. The type checker turns it into `checked_command`/`checked_expr`, whose constructors have a `C` prefix. Checked nodes are normalized: declarations without initializers get default values, and `Declare`/`Assign` become lists of pairs. A whole program is a single `command`, a chain of `Seq`. Function and procedure definitions are commands too.
- **Locations.** Every node is wrapped in `Loc.t = { it; at }` (`lib/loc`). Errors are `string Loc.t` all the way to the CLI and the web UI, which highlights the location.
- **Syntax errors.** The parser uses Menhir's table back-end and incremental API (`MaoLang.parse`). A syntax error is reported on the rejected token, with a hint: first from `token_hint` in `maoLang.ml` (tokens such as `=`, `?` and `return` that are wrong wherever they are rejected), otherwise from `maoParser.messages`, which maps parser states to messages and is compiled by `menhir --compile-errors`. Find the state of an example sentence with `echo 'program: IDENT DECLARE' | menhir --interpret-error lib/maoLang/maoParser.mly`. Grammar changes can renumber states: the build fails if a sentence no longer ends in an error or two sentences reach the same state.
- **Type checker** (`maoTypeChecker.ml`) returns `Result`. Function and procedure bodies are checked in an environment that holds **only the definition itself and its parameters**: no access to outer variables, recursion allowed, nested definitions allowed. Because of this, the interpreter can safely run calls in the caller's environment.
- **Interpreter** (`maoInterpreter.ml`) runs in the reader-state-result monad (`lib/readerStateResultMonad`, aliased `RSR`). The read-only environment holds the `limits` (`max_logs`, `max_depth`), read with `RSR.ask`. The state accumulates logs and tracks `log_id`, `call_id` and call `depth`. It provides `let*`, `>>=`, `=>>` (map) and `=&>` (map_error). Two kinds of failure:
  - Genuine runtime errors go through `RSR.error`: integer division by zero, index out of bounds, casting a double that is not representable as an int, the step limit (`max_logs`) and the recursion limit (`max_depth`). Doubles follow IEEE 754, so `1.0 / 0.0` is `+∞`.
  - States the type checker rules out raise `Utils.Unreachable` through `unreachable "..."`. `MaoLang.run` catches it and reports `Errore interno dell'interprete`. In tests this outcome is `Crash`.
- **Runtime model** (`maoDataStructure.ml`):
  - `Env` maps a name to a `binding`, which is either `Addr`, `LambdaFun` or `LambdaProc`.
  - `Mem` is a persistent map from `Int64Addr` to `RTVal.t`.
  - An array value is `RTVal.Addr arr_ptr`. The cell at `arr_ptr` holds the length, and element `i` is at `arr_ptr + 1 + i`.
  - Memory is never freed. On leaving a block the outer `env` comes back but the new `mem` is kept (`replace_mem`).
- **Trace.** Each log entry is `{ state; event } Loc.t` and holds a full snapshot of the state, which is cheap because env and memory are immutable. `maoTrace.ml` builds paging, filtering by `Kind`, and the variable and memory views on top of the logs array.
- **Web** (`web/`):
  - `main.ml` compiles to WASM and installs a global `Mao` object (`run`, `format`, `filter`, `logs`, `position`, `env`, `memory`) that converts OCaml values to JS objects (`ToJs`).
  - `worker.js` loads it in a Web Worker and answers RPC messages, turning JS stack overflows into an Italian message.
  - `app.js` is the hand-written UI. It has no framework; Prism is vendored in `web/vendor/`.
  - `build_site.sh` copies every `sample/*.mao` into `_site/samples/` and generates `index.json`, so a new sample shows up in the UI's examples menu automatically.

A new language construct usually touches every layer: `maoAst.mli` (both ASTs), `maoLexer.mll`, `maoParser.mly` (and possibly `maoParser.messages`), `maoAst_pp.ml` (the formatter must print it back as valid syntax), `maoTypeChecker.ml` and `maoInterpreter.ml`. If it logs something, it also touches `event` in `maoDataStructure.ml`, `maoDataStructure_pp.ml`, `Trace.Kind` and possibly `web/main.ml`/`app.js`. Add tests for each layer.

## Tests

`test/` uses alcotest with a single runner, `test.ml`. Each `testX.ml` exports `tests : unit Alcotest.test_case list`, and `test.ml` lists them as one group per module, so **a new test file must be added to `test.ml`**. Shared helpers live in `testCommon.ml`:

- `test name f` is ``Alcotest.test_case name `Quick f``.
- `rtval` is a testable for runtime values. `int`, `bool`, `double` and `char` build values.
- `check_values src [(id, v)]` checks final variable values.
- `check_array src id vs` checks the contents of an array.
- `check_fails ParseError | TypeError | RuntimeError src` checks how a program fails.
- `run_program` runs a source string through the whole pipeline.
- `run_traced src` runs a program that passes the static checks and returns its result and logs.

Tests are written as MAO source strings, and the check message is usually the source itself. Group related assertions in one test case, and use `List.iter` over a table of inputs.

## OCaml conventions

Follow the style of `maoInterpreter.ml`, `maoTypeChecker.ml`, `maoDataStructure.ml`, `maoAst_pp.ml` and `utils.ml`.

- `open` the project modules at the top (`MaoAst`, `MaoAst_pp`, `MaoDataStructure`, `Utils`) and use short aliases such as `module RSR = ReaderStateResultMonad`.
- Group types and records in namespacing submodules (`module RTVal = struct ... end`, `MemMap.Make` functors) instead of prefixing field names.
- Destructure in parameters: `Loc.{ it = e; at = loc }`, `{ env; mem }`, `fun { mem } -> mem`.
- Pipelines use `|>`. Composition uses `>>` and `***` from `Utils`.
- Pretty-print with `Fmt`: `pp_*` functions, `Fmt.pf fmt`, `Fmt.str "%a"`, `Fmt.list ~sep:(Fmt.any ", ")`. Build error messages with `Fmt.kstr`, as `error_with_loc ~loc fmt` does.
- Write in a purely functional style: pure functions that take and return state explicitly, with effects such as errors, logging and counters kept inside monadic computations (`Result`, `ReaderStateResultMonad`), not side effects.
- Prefer immutable code and stdlib combinators (`List.find_map`, `Seq`, `Array.fold_left_map`, the `Utils.result_fold_*M` helpers) over refs and mutable arrays.
- Use domain-driven snake_case names: `arr_ptr`, `arr_len`, `call_id`, and `c_e`/`c_c` for checked nodes.
- Keep comments rare and short, only where something is not obvious.
- Run `dune fmt` before finishing. `.ocamlformat` pins version 0.29.0 with the default profile.

## Web UI

The interface is in Italian and must stay simple for students. Put no explanatory text in `web/index.html`: all help text lives in `web/info.html`, opened by the "i" button. The maintainer edits `info.html` by hand, so don't overwrite their changes.
