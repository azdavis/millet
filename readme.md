# Millet

An implementation of Standard ML being developed mainly for use in
[15-150][150], Carnegie Mellon University's introductory functional programming
course.

## Development

First install Rust and Cargo, probably with [rustup][rustup].

Then clone the repository, cd inside, and run `cargo build`.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for
15-150.

## Goals

Goals are listed in order of importance.

### Be correct

Bugs in a program are never desirable. They are, however, _especially_
undesirable when the program is intended for use by students, many of whom are
first being introduced to functional programming. We must, therefore, strive to
implement the [Definition of Standard ML correctly][sml-def].

### Have good error messages

In the past, 15-150 has used [Standard ML of New Jersey][smlnj], which
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

[smlnj]: https://www.smlnj.org
[150]: http://www.cs.cmu.edu/~15150/
[sml-def]: https://smlfamily.github.io/sml97-defn.pdf
[rustup]: https://rustup.rs
