# Millet

An set of tools for Standard ML.

Right now, the set has only one element: a language server for SML, and a
corresponding Visual Studio Code language client extension.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for
[15-150][one-fifty], Carnegie Mellon's introductory functional programming
course.

## Development

First install Rust, probably with [rustup][].

Then clone the repository, cd inside, and run `cargo build`.

## Testing

A test is a directory directly inside the directory `tests`.

If the test contains a file `run.sh`, then when that file is run with
`sh -eu run.sh`, it must produce `out.tmp` inside the test. The test must also
contain `out.txt` which contains the expected contents of `out.tmp`.

Else, if the test contains a file `err.sml`, then when the Millet CLI is run
with that file, it must exit with a non-zero exit code. The test must also
contain `out.txt` which contains the expected output of the Millet CLI when run
on this file.

Else, if the test contains a file `ok.sml`, then when the Millet CLI is run with
that file, it must exit with the exit code 0.

Run `bin/run-test tests/<name>` to run a test.

After you first create a test, or if you update a test, you may want to generate
an appropriate `out.txt` file. Use `bin/run-test -g tests/<name>` to do that.

[one-fifty]: http://www.cs.cmu.edu/~15150/
[rustup]: https://rustup.rs
