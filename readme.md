# Millet

An implementation of Standard ML being developed mainly for use in
[15-150][one-fifty], Carnegie Mellon University's introductory functional
programming course.

The main project under current development is a language server, and a
corresponding Visual Studio Code language client extension. Other projects like
an automatic formatter, an interpreter, a debugger, and a compiler might be
explored.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for
15-150.

## Development

First install Rust and Cargo, probably with [rustup][].

Then clone the repository, cd inside, and run `cargo build`.

## Testing

To make a new test, run `bin/mk-test tests/<name>` from the top level of the
repo.

A test is a directory with the following properties.

- The parent of the test must be the directory `tests`.
- The test must contain a file called `run.sh`, which when run with
  `sh -eu run.sh`, must produce `out.tmp` directly inside the test.
- The test must also contain a `out.txt` file that contains the expected
  contents of `out.tmp`.

Run `bin/run-test tests/<name>` to run a test. The test runner will run
`sh -eu run.sh` to produce `out.tmp` and diff it against `out.txt`. The test
will be marked as failing if there is a difference. The test will also be marked
as failing if `sh -eu run.sh` exits with a non-zero exit code.

After you first create a test, or if you update a test, you may want to generate
an appropriate `out.txt` file. Use `bin/gen-test-out tests/<name>` to do that.

[one-fifty]: http://www.cs.cmu.edu/~15150/
[rustup]: https://rustup.rs
[sml-def]: https://smlfamily.github.io/sml97-defn.pdf
[sml-nj]: https://www.smlnj.org
