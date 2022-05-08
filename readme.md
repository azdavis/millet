# Millet

A [language server][lang-server] for [Standard ML][sml-def].

Also a corresponding [Visual Studio Code][vscode] extension.

## Warning

This project is alpha-quality software.

- There are [many important things](todo.md) not yet implemented.
- There are things that are shoddily implemented. Note the skipped tests in
  `tests/`.

## Development

`git clone` the repo, `cd` inside, and run `scripts/setup.sh`.

If you're using VSCode, you can try out the VSCode extension:

1. Open the root directory of this repository in VSCode
2. Open the Run panel from the activity bar (the play button with bug)
3. Select 'extension' in the drop down
4. Press the green play button

## Repository layout

- Mode of the code is in `crates/`.
- Editor extensions are in `extensions/`.
- Tests are in `tests/` (who'd have guessed?).
- Various helper scripts are in `scripts/`. They require a POSIX `sh`.

## Testing

A test is a directory directly inside the directory `tests`. Run a test with
`scripts/run-test.sh`.

A test must be of the given format:

1. If the test contains an empty file `skip`, then the test is skipped.
2. Else, if the test contains a file `run.sh`, then when that file is run with
   `sh -eu run.sh`, it must exit 0. (See below.)
3. Else, if the test contains a file `ast.sml`, then when the Millet CLI is run
   to output AST for that file, it must exit 0 and produce the output in
   `out.txt`.
4. Else, if the test contains a file `ok.sml`, then when the Millet CLI is run
   with that file on quiet mode, it must exit 0 and produce no output.
5. Else, if the test contains a file `err.sml`, then when the Millet CLI is run
   with that file, it must exit 1 and produce the output in `out.txt`.
6. Else, the test is invalid.

Run `scripts/mk-test.sh tests/<name>` to make an `ok.sml` test.

When writing a custom `run.sh` test, the test runner exports two environment
variables available for use in any `run.sh`:

- `NO_COLOR=1`, to turn off colored output when running the Millet CLI.
- `MILLET`, which is an absolute path to the Millet CLI binary.

Use the `-g` flag of `scripts/run-test.sh` to generate the expected output from
the actual output for a test. This helps when you first write a test or update a
test and want to change the expected files.

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
