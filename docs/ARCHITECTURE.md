# Architecture

This is a high-level [architecture][] of Millet. Millet is a language server for Standard ML (SML), written in Rust.

A [language server][ls] is a long-running, stateful process that transforms a repository of code, and the client's edits to that code, into a semantic model about that code that can be queried by the client.

[SML][sml] is a functional programming language, formally defined by its [Definition][defn]. It emphasizes modularity, purity, and formal reasoning about programs. It is often used in educational contexts to teach students the basics of functional programming.

[Rust][rust] is a programming language with an emphasis on reliability and efficiency. Being written in Rust, Millet is split into several, somewhat modular "crates".

## Type crates

These crates provide foundational types that many other crates use.

### `crates/sml-syntax`

SML tokens and lossless concrete syntax trees (CST), and abstract syntax tree (AST) wrapper API. Depends on [rowan][], a library for CSTs.

"Lossless" means the exact input source file can be reproduced from its CST.

In a conventional compiler, source code is often first lexed into tokens, then those tokens are parsed into ASTs. Often, by design, these AST types do not allow for representing partially-formed programs. For example, we could represent `1 + 3` or `foo(3, 5) - bar(4)` with an AST, but not `1 +` or `foo(3,`.

This is fine for the regular compiler use case; in fact, it is **desired**, since a compiler should not compile malformed, incomplete programs. But for the language server use case, this is unacceptable, because the client is often in the middle of editing their code, and therefore their code is syntactically invalid. A language server should, in spite of this, strive to understand as much about the (possibly syntactically invalid) code as possible.

Thus, since the AST wrapper API provides a "view" into a concrete syntax tree, and that concrete tree may be partial and/or malformed, pretty much all "fields" exposed by the AST API are optional.

Mostly generated from its [ungrammar][].

### `crates/sml-hir`

High-level Intermediate Representation.

Uses arenas, and indices into those arenas, to represent recursive structure. For instance, instead of the usual

```rs
enum Exp {
  Num(u32),
  Str(String),
  Fn(Var, Box<Exp>),
  App(Box<Exp>, Box<Exp>),
}
```

We'd represent it (almost) like this:

```rs
/// an arena of Exp allocations
type ExpArena = Arena<Exp>;
/// an index into that arena
type ExpIdx = Idx<Exp>;

enum Exp {
  Num(u32),
  Str(String),
  Fn(Var, ExpIdx),
  App(ExpIdx, ExpIdx),
}
```

The "almost" is because: since we need to represent partial nodes (from a partial parse), HIR nodes need to be able to contain "empty" sub-nodes. So we make just one change to the above:

```rs
/// an index might not actually be there if the sub-node couldn't be lowered
type ExpIdx = Option<Idx<Exp>>;
```

### `crates/sml-mir`

Mid-level Intermediate Representation.

Used for the dynamics.

### `crates/sml-lab`

`Lab`, a label for tuples/records.

### `crates/sml-scon`

`SCon`, a special value constructor (aka "literal") for primitive types (`int`, `string`, etc).

### `crates/sml-path`

A type representing a "path", aka a non-empty sequence of names.

### `crates/sml-fixity`

Types relating to SML infix operators.

### `crates/sml-namespace`

Types representing various SML namespaces.

### `crates/sml-statics-types`

Important types and operations on those types for static analysis.

This used to be a part of statics itself, but the types involved for statics are complicated and plentiful enough that this was made its own crate.

## Passes crates

These crates are the main "passes" on SML code. Together they form essentially a SML compiler frontend.

Notably, most passes that can produce errors have the approximate shape

```rs
Input -> (Output, Vec<Error>)
```

instead of

```rs
Input -> Result<Output, Error>
```

That is to say, we always produce _both_ as best an output we could make _and_ as many errors as we could find, as opposed to _either_ a "perfect" output _or_ the first error we encountered.

The latter is more common in "regular" compilers, but we're a language server. In the case of a language server, we want to analyze as much of the code as possible as far as possible, even if the information we have is imperfect.

### `crates/input`

```rs
&std::path::Path -> (Input, Vec<InputError>)
```

Get input from a filesystem.

The most important thing in the input is a mapping from file names to either:

- raw (string) contents, for SML source files
- parsed and processed contents, for CM/MLB "group" files

### `crates/sml-lex`

```rs
&str -> (Vec<sml_syntax::Token<'_>>, Vec<LexError>)
```

Lex (aka tokenize) a string of an SML program into tokens.

### `crates/sml-parse`

```rs
&[sml_syntax::Token<'_>] -> (sml_syntax::ast::Root, Vec<ParseError>)
```

Parses a sequence of tokens into a sequence of "events". Events are like:

- start a node
- consume a token
- emit an error
- finish a node

Then processes those events to build a lossless syntax tree, wrapped in the AST API.

### `crates/sml-hir-lower`

```rs
&sml_syntax::ast::Root -> (sml_hir::Root, TwoWayPointers, Vec<LowerError>)
```

Lowers ("elaborates") AST into HIR.

When lowering, we turn complex constructs into more fundamental ones. For instance:

```sml
  a andalso b
= if a then b else false
= case a of true => b | false => false
= (fn true => b | false => false) a
```

We also construct a two-way mapping between HIR indices and "pointers" to AST nodes. Each direction has a use:

- HIR -> AST: for recovering the text range for which to report an error for a HIR index.
- AST -> HIR: for knowing what HIR index is e.g. being hovered over by the client.

### `crates/sml-ty-var-scope`

```rs
sml_hir::Root -> sml_hir::Root
```

Handles adding implicitly-scoped type variables to their `val` declaration/specification binding sites.

### `crates/sml-statics`

```rs
(State, Basis, sml_hir::Root) -> (State, Basis, Info, Vec<StaticsError>)
```

Does static analysis ("typechecking") on HIR, according to the statics semantics of SML from its Definition.

The other passes thus far operate on single files, but this one is meant to be run "across" many files. So we take in and give out updated state.

This crate contains the implementation of most of the rules of the static semantics for SML from the Definition. (Some trivial ones like the rules for parenthesized expressions are handled implicitly when lowering). Relevant code is tagged with the special comment `@def(N)` for statics rule N.

Statics errors use an abstract `Idx`, and this index gets turned into an actual text range with the `TwoWayPointers` from lower.

In the future we could add more to this `Idx`, like "the name of the third con bind in the second dat bind of this datatype dec".

### `crates/sml-mir-lower`

Lowers HIR to MIR, for the dynamics.

### `crates/sml-dynamics`

Runs a program, according to the dynamic semantics of SML from its Definition. The program should have been typechecked with the statics already.

## SML-adjacent helper crates

These crates do interesting things related to SML files, but aren't really full "passes".

### `crates/sml-naive-fmt`

Naively format SML files.

Doesn't handle comments or line length.

### `crates/sml-comment`

Extract interesting comments from above SML syntax nodes, like doc comments.

### `crates/lex-util`

Some common lex utilities used in multiple lexing crates, like:

- Handling `(* ... *)` style block comments with nesting.
- Handling SML's definition of whitespace. (We actually consider a superset of the SML definition to be whitespace.)

### `crates/slash-var-path`

Handle paths with `$(FOO)` or `$BAR` style variables in them, as in MLB and CM files.

## SML group file crates

These crates are related to "group files" in the SML ecosystem, namely SML/NJ Compilation Manager and ML Basis.

### `crates/cm-syntax`

Processes the syntax of SML/NJ Compilation Manager (`.cm`) files.

### `crates/mlb-syntax`

Processes the syntax of ML Basis (`.mlb`) files into AST values.

### `crates/mlb-hir`

HIR for MLB.

### `crates/mlb-statics`

Static semantics for MLB files.

Because the semantics for MLB files determines when source (SML) files get parsed and statically analyzed, this depends on most of the crates that analyze SML.

## Overall crates

These crates depend on many other crates to "unite" everything together.

### `crates/sml-file-syntax`

Unites all of the purely syntactical SML passes into one single API.

### `crates/analysis`

Unifies ~all of the passes into one single API.

### `crates/lang-srv`

Depends on `analysis` and a bunch of third party crates to implement a language server based on `analysis`. Has a fair amount of "glue code" to convert between analysis types and LSP types.

## Other crates

These crates don't really fit in anywhere else.

### `crates/panic-hook`

A good default panic hook that shows backtraces and exhorts the reader to file a bug report.

### `crates/chain-map`

A map optimized for the use-case of cloning and adding a few elements onto the clone. Used in the statics. Named after Python's [collection][].

[collection]: https://docs.python.org/3/library/collections.html#collections.ChainMap

### `crates/config`

The format for the optional Millet configuration file.

### `crates/tests`

All the tests for all of the other crates. Depends on `analysis`, and consumes its public API to test functionality of each of the 'passes'.

No crate except this crate is expected to have tests, and this crate is expected to have nothing but tests.

Test case are usually SML programs, which contain "expectation comments" asserting that `analysis` should behave a certain way about a certain region of the program.

```rs
use crate::check::check;

#[test]
fn undefined() {
  check(
    r#"
val _ = nope
(**     ^^^^ undefined value: nope *)
"#,
  );
}
```

## Binary crates

These crates are the only ones that produce executable binaries. Every other crate is only compiled to a library.

### `crates/millet-ls`

Depends on `lang-srv` and actually runs it.

### `crates/millet-cli`

A thin CLI wrapper around `analysis`. It basically does one full analysis of the input, prints any errors to stdout, and exits, much like a conventional compiler or linter.

## Other code

Most of the code is contained in the Rust crates documented above, but some code lives elsewhere.

### `docs`

Documentation, like this!

### `editors`

Support for specific text editors via language client extensions/"glue code".

### `editors/vscode`

The VS Code client extension, in [TypeScript][ts].

### `xtask`

A [task runner][xtask] written in Rust.

Allows you to invoke `cargo xtask <task>` to run the `<task>`.

### `.cargo`

Configuration for Cargo, Rust's package manager and build tool. Allows `cargo xtask` to work.

### `Cargo.toml`

At the top level, this sets up a "Cargo workspace" of all the crates in `crates` and `xtask`. Each crate also has its own `Cargo.toml` defining fundamental things about the crate like its name and dependencies.

### `Cargo.lock`

A lockfile for Cargo that records the exact versions of all dependencies that Cargo resolved.

### `rustfmt.toml`

Configuration for rustfmt, Rust's automatic code formatter. Before checking code into git, it must be formatted by rustfmt.

### `.github`

Configuration for GitHub, like:

- PR templates
- Issue templates
- How to run CI on GitHub Actions

### `.gitignore`

Configuration for git, to ignore generated files like `/target` (the output folder for Rust).

### `.vscode`

Configuration for VS Code, like:

- How to launch an instance of VS Code with a local version of the extension for testing
- What files to ignore in the sidebar (e.g. `/target`)

[architecture]: https://matklad.github.io/2021/02/06/ARCHITECTURE.md.html
[defn]: https://smlfamily.github.io/sml97-defn.pdf
[ls]: https://microsoft.github.io/language-server-protocol/
[rowan]: https://github.com/rust-analyzer/rowan
[rust]: https://www.rust-lang.org
[sml]: https://smlfamily.github.io
[ts]: https://www.typescriptlang.org
[ungrammar]: https://github.com/rust-analyzer/ungrammar
[xtask]: https://github.com/matklad/cargo-xtask
