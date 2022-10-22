# Architecture

Millet is a language server for SML. A language server is a long-running, stateful process that transforms a repository of code, and the client's edits to that code, into a semantic model about that code that can be queried by the client.

## Code map

### `crates/sml-syntax`

SML concrete syntax (like tokens), and AST wrapper API.

Depends on [rowan][], a library for lossless concrete syntax trees. "Lossless" means we can represent partial parses with no information loss, which is important for the IDE use case since the code is often in the middle of being edited by the client and therefore syntactically invalid.

Thus, since the AST wrapper API provides a "view" into a concrete syntax tree, and that concrete tree may be partial and/or malformed, pretty much all "fields" exposed by the AST API are optional.

Mostly generated from its [ungrammar][].

### `crates/sml-hir`

High-level intermediate representation. Uses arenas, and indices into those arenas, to represent recursive structure.

For instance, instead of the usual

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

### `crates/sml-lex`

```rs
String -> (Vec<sml_syntax::Token>, Vec<LexError>)
```

Note that for this and all of the rest of the "passes", the "signatures" given, like the one above, are not exact.

Lexes ("tokenizes") a string into tokens.

Note that the signature is _not_ this:

```rs
String -> Result<Vec<sml_syntax::Token>, Vec<LexError>>
```

That is to say, we always produce _both_ whatever tokens we could _and_ as many errors as we could find. This pattern is common across the rest of the "passes".

The idea is: we want to analyze as much of the code as possible as far as possible, even if the information we have is imperfect.

### `crates/sml-parse`

```rs
Vec<sml_syntax::Token> -> (sml_syntax::ast::Root, Vec<ParseError>)
```

Parses a sequence of tokens into a sequence of "events". Events are like:

- start a node
- consume a token
- emit an error
- finish a node

Then processes those events to build a lossless syntax tree, wrapped in the AST API.

### `crates/sml-lower`

```rs
sml_syntax::ast::Root -> (sml_hir::Root, TwoWayPointers, Vec<LowerError>)
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
(sml_statics::State, sml_hir::Root) -> (sml_statics::State, Vec<StaticsError>)
```

Does static analysis ("typechecking") on HIR.

The other passes thus far operate on single files, but this one is meant to be run "across" many files. So we take in and give out updated state.

Statics errors use an abstract `Idx`, and this index gets turned into an actual text range with the `TwoWayPointers` from lower.

NOTE: In the future we could add more to this `Idx` (maybe call it `Entity`), like "the name of the third con bind in the second dat bind of this datatype dec".

### `crates/sml-fmt`

Naively format SML files.

### `crates/sml-comment`

Extract interesting comments from above SML syntax nodes, like doc comments.

### `crates/cm-syntax`

Processes the syntax of SML/NJ Compilation Manager (`.cm`) files.

### `crates/mlb-syntax`

Processes the syntax of ML Basis (`.mlb`) files into AST values.

### `crates/mlb-statics`

Static semantics for MLB files.

Because the semantics for MLB files determines when source (SML) files get parsed and statically analyzed, this depends on most of the crates that analyze SML.

### `crates/config`

The format for the optional Millet configuration file.

### `crates/paths`

Types for working with paths, notably:

- A wrapper type for `PathBuf` that guarantees the inner `PathBuf` is canonical.
- A type that transforms these canonical path buffers into cheap IDs, given that a path is "contained" in a "root" canonical path buf.

These are ideal for the use case of language servers, in which we have a "workspace root" containing all the files.

### `crates/lex-util`

Some common lex utilities used in multiple lexing crates, like:

- Handling `(* ... *)` style block comments with nesting.
- Handling whitespace.

### `crates/str-util`

Some common string utilities, like:

- Small strings, just a re-export of `smol_str::SmolStr`.
- Names, aka non-empty `SmolStr`s.

### `crates/text-size-util`

A wrapper around the `text-size` crate to add some helpers, primarily `WithRange`, a pair of a value and a text range.

### `crates/fmt-util`

A small utility crate for formatting.

### `crates/diagnostic-util`

A small crate defining primarily the overall `Error` type, which Millet reports to a language client.

### `crates/fast-hash`

A thin wrapper over `FxHash{Map, Set}` with some extra helper functions. These types use `FxHasher`, which is a very fast, but not HashDOS-resistant, hashing algorithm used in Firefox and `rustc`.

### `crates/elapsed`

A small utility crate for timing function calls.

### `crates/analysis`

Unifies all the passes into one single API.

### `crates/lang-srv`

Depends on `analysis` and a bunch of third party crates to implement a language server. This is one of two binary targets. Note that only binary targets may perform IO.

### `crates/cli`

A CLI wrapper around `analysis`. It basically does one full analysis of the input, prints any errors to stdout, and exits, much like a conventional compiler or linter.

### `crates/tests`

The tests. Depends on `analysis`, and consumes its public API to test functionality of each of the 'passes'.

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

See the documentation of `check()` for how to write tests.

### `docs`

Documentation, like this!

### `editors`

Support for specific text editors. Right now there is only one.

### `editors/vscode`

The VS Code client extension, in TypeScript.

### `xtask`

A [task runner/"build system"][xtask] written in Rust.

Allows you to invoke `cargo xtask <task>` to run the `<task>`.

### `.cargo`

Configuration for Cargo, Rust's package manager and build tool. Allows `cargo xtask` to work.

### `.github`

Configuration for GitHub, like:

- PR templates
- Issue templates
- The CI job run, run o GitHub Actions.

### `.vscode`

Configuration for VS Code, like:

- How to launch an instance of VS Code with a local version of the extension for testing
- What files to ignore in the sidebar (e.g. `/target`)

[xtask]: https://github.com/matklad/cargo-xtask
[rowan]: https://github.com/rust-analyzer/rowan
[ungrammar]: https://github.com/rust-analyzer/ungrammar
