# Millet

A [language server][lang-server] for [Standard ML][sml-def], with a
corresponding [Visual Studio Code][vscode] language client extension.

This project is alpha-quality software. There are
[many important things](todo.md) not yet implemented. There are things that are
shoddily implemented (note the skipped tests in `tests/`).

## Development

First, get the repository, probably by cloning with `git`, and cd inside.

For development of the main codebase in `crates`, install Rust, probably with
[rustup][]. Then run `cargo build`.

For development of the VSCode extension in `extensions/vscode`, first install
[node][]. Then cd into `extensions/vscode`, and run `npm install && npm build`.

If you're using VSCode, open the root directory of this repository in VSCode to
get extension recommendations for a pleasant Rust developer experience.

VSCode also lets you try out the language client extension from the Run panel.
Open the Run panel (play button with bug), select 'extension' in the drop down,
and press the green play button. This will build the language server and client
(it might take a bit), and then open a new VSCode window with the extension
enabled. From there you can open up a SML file and try it out.

The scripts in `scripts` require a POSIX `sh`.

## Repository layout

- `.vscode` contains configuration for Visual Studio Code, which is invaluable
  when developing the VSCode language client extension.
- `scripts` contains various scripts.
  - `scripts/ck-sml-defn.sh` checks whether all of the rules of the statics have
    been mentioned in the implementation of the statics.
  - `scripts/mk-test.sh` makes `ok.sml` tests.
  - `scripts/mk-vscode-ext.sh` builds the VSCode language client extension and
    language server, and puts the language server binary near the built client.
  - `scripts/run-test.sh` runs tests in `tests`.
- `crates` is the primary location of code implementing Standard ML.
  - `crates/cli` contains a command-line interface which runs the lexer, parser,
    and typechecker from `crates/base` on a sequence of files.
  - `crates/base` contains the Standard ML lexer, parser, and typechecker.
  - `crates/ls` contains a language server which runs the lexer, parser, and
    typechecker from `crates/base` on files sent to it by the language client.
- `doc` contains documentation.
- `extensions` contains language client extensions for text editors to
  communicate with the language server.
- `tests` contains tests.

## Testing

A test is a directory directly inside the directory `tests`.

If the test contains an empty file `skip`, then the test is skipped.

Else, if the test contains a file `run.sh`, then when that file is run with
`sh -eu run.sh`, it must exit 0.

Else, if the test contains a file `ast.sml`, then when the Millet CLI is run to
output AST for that file, it must exit 0 and produce the output in `out.txt`.

Else, if the test contains a file `ok.sml`, then when the Millet CLI is run with
that file on quiet mode, it must exit 0 and produce no output.

Else, if the test contains a file `err.sml`, then when the Millet CLI is run
with that file, it must exit 1 and produce the output in `out.txt`.

Run `./scripts/mk-test.sh tests/<name>` to make an `ok.sml` test, and run
`./scripts/run-test.sh tests/<name>` to run a test. The test runner exports two
environment variables available for use in any `run.sh`:

- `NO_COLOR=1`, to turn off colored output when running the Millet CLI.
- `MILLET`, which is an absolute path to the Millet CLI binary.

After you first create a test, or if you update a test, you may want to generate
an appropriate `out.txt` file. Use `./scripts/run-test.sh -g tests/<name>` to do
that.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for
[15-150][cmu150], Carnegie Mellon's introductory functional programming course
taught in Standard ML.

[cmu150]: http://www.cs.cmu.edu/~15150/
[lang-server]: https://microsoft.github.io/language-server-protocol/
[node]: https://nodejs.org/en/
[rustup]: https://rustup.rs
[sml-def]: https://smlfamily.github.io/sml97-defn.pdf
[vscode]: https://code.visualstudio.com
