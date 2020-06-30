# Millet

An implementation of Standard ML being developed mainly for use in
[15-150][one-fifty], Carnegie Mellon University's introductory functional
programming course.

## Goals

Goals are listed in order of importance.

### Be correct

Bugs in a program are never desirable. They are, however, _especially_
undesirable when the program is intended for use by students, many of whom are
first being introduced to functional programming. We must, therefore, strive to
implement the [Definition of Standard ML][sml-def] correctly.

### Have good error messages

In the past, 15-150 has used [Standard ML of New Jersey][sml-nj], which
unfortunately has error messages that can be at times hard to understand. To
provide a great experience for our students, it is important to have great error
messages.

### Have good tooling

Modern tooling helps the programmer stay productive. Editor integration via a
language server reduces context switching. An automatic code formatter reduces
cognitive load by de-necessitating manual fiddling with indentation.

### Be easy to install and run

Students have many different devices and operating systems, so it should be easy
to install and run Millet and its associated tooling on many popular platforms.

### Be complete

We should strive to implement the entire spec of Standard ML. However, there are
many features of Standard ML not used by 15-150, so those features are not as
important to implement.

### Be fast

The programs we ask students to write are not performance-critical, and are
generally very short-lived. It is probably sufficient to implement the dynamic
semantics with an interpreter, and not worry about compilation, which would
probably result in faster-running code.

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
