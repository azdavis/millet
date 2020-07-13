# Millet

A set of tools for Standard ML.

Right now, the set has only one element: a [language server][lang-server], with
a corresponding [Visual Studio Code][vscode] language client extension.

This project is alpha-quality software. It is nearing MVP status, but is not yet
there. There are many important things not yet implemented. See TODO.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for
[15-150][one-fifty], Carnegie Mellon's introductory functional programming
course.

## Development

First, clone the repository and cd inside.

For development of the main codebase in `crates`, install Rust, probably with
[rustup][]. Then run `cargo build`.

For development of the VSCode extension in `extensions/vscode`, first install
[node][]. Then cd into `extensions/vscode`, and run `npm install`.

If you're using VSCode, open the root directory of this repository in VSCode to
get extension recommendations for a pleasant Rust developer experience.

VSCode also lets you try out the language client extension from the Run panel.
Open the Run panel (play button with bug), select 'extension' in the drop down,
and press the green play button.

Some scripts in `bin/` require either `sh` (POSIX) or `python3`.

## Repository layout

- `.vscode` contains configuration for Visual Studio Code, which is invaluable
  when developing the VSCode language client extension.
- `bin` contains various scripts.
  - `bin/ck-sml-defn` checks whether all of the rules of the statics have been
    mentioned in the implementation of the statics.
  - `bin/mk-vscode-ext` builds the VSCode language client extension and language
    server, and puts the language server binary near the built client.
  - `bin/run-test` runs tests in `tests`. See below.
- `crates` is the primary location of code implementing Standard ML.
  - `crates/cli` contains a CLI interface which runs the lexer, parser, and
    typechecker from `crates/core` on a sequence of files.
  - `crates/core` contains the Standard ML lexer, parser, and typechecker.
  - `crates/ls` contains a language server which runs the lexer, parser, and
    typechecker from `crates/core` on files sent to it by the language client.
- `extensions` contains language client extensions for editors to communicate
  with the language server.
- `tests` contains tests. See below.

## Testing

A test is a directory directly inside the directory `tests`.

If the test contains a file `run.sh`, then when that file is run with
`sh -eu run.sh`, it must exit 0.

Else, if the test contains a file `ast.sml`, then when the Millet CLI is run to
output AST for that file, it must exit 0 and produce the output in `out.txt`.

Else, if the test contains a file `ok.sml`, then when the Millet CLI is run with
that file on quiet mode, it must exit 0 and produce no output.

Else, if the test contains a file `err.sml`, then when the Millet CLI is run
with that file, it must exit with a non-zero exit code and produce the output in
`out.txt`.

Run `bin/run-test tests/<name>` to run a test.

After you first create a test, or if you update a test, you may want to generate
an appropriate `out.txt` file. Use `bin/run-test -g tests/<name>` to do that.

## TODO

- statics for type variables
  - implicit scoping
  - performing closure operations
  - checking whether expressions are expansive
- better statics for equality types
- statics for rest of modules
  - environment matching
  - functors
- parse derived forms
  - functor binding input sugar
  - specifications
  - signature expressions
  - programs
- statics for unused constructs (abstype, while, selectors)?
- make public
  - turn on protected branches, etc
  - use github actions ci
- publish extension
  - get azure account or whatever
- better error messages
  - prefer 'expected int list, found bool list' instead of 'expected int, found
    bool' and similar?
  - show fully qualified names in type errors
  - better overload errors: maybe we should choose what that overloaded type
    should be sooner
  - better parser errors?
- handle CM? (guh)
- more LSP features
  - jump to def
  - hover for type
  - many files, maybe via `millet.json` in the workspace root

[one-fifty]: http://www.cs.cmu.edu/~15150/
[rustup]: https://rustup.rs
[lang-server]: https://microsoft.github.io/language-server-protocol/
[vscode]: https://code.visualstudio.com
[node]: https://nodejs.org/en/
