# Architecture

Millet is a language server for SML. A language server is a long-running, stateful process that transforms a repository of code, and the client's edits to that code, into a semantic model about that code that can be queried by the client.

## Code map

### `xtask`

A [task runner/"build system"][xtask] written in Rust.

### `crates/syntax`

SML concrete syntax (like tokens), and AST wrapper API.

Depends on [rowan][], a library for lossless concrete syntax trees. "Lossless" means we can represent partial parses with no information loss, which is important for the IDE use case since the code is often in the middle of being edited by the client and therefore syntactically invalid.

Thus, since the AST wrapper API provides a "view" into a concrete syntax tree, and that concrete tree may be partial and/or malformed, pretty much all "fields" exposed by the AST API are optional.

Mostly generated from its [ungrammar][].

### `crates/hir`

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

### `crates/lex`

Lexes ("tokenizes") a string into tokens. Basically this:

```
String -> (Vec<syntax::Token>, Vec<LexError>)
```

Note that it's _not_ this:

```
String -> Result<Vec<syntax::Token>, Vec<LexError>>
```

That is to say, we always produce _both_ whatever tokens we could _and_ as many errors as we could find. This pattern is common across the rest of the "passes".

The idea is: we want to analyze as much of the code as possible as far as possible, even if the information we have is imperfect.

### `crates/parse`

Parses a sequence of tokens into a sequence of "events". Events are like:

- start a node
- consume a token
- emit an error
- finish a node

Then processes those events to build a lossless syntax tree, wrapped in the AST API.

Basically this:

```
Vec<syntax::Token> -> (syntax::ast::Root, Vec<ParseError>)
```

### `crates/lower`

Lowers ("elaborates") AST into HIR.

When lowering, we turn complex constructs into more fundamental ones. For instance:

```
    if a then b else c
==> case a of true => b | false => c
==> (fn true => b | false => c) a
```

We also construct a two-way mapping between HIR indices and "pointers" to AST nodes. Each direction has a use:

- HIR -> AST: for recovering the text range for which to report an error for a HIR index.
- AST -> HIR: for knowing what HIR index is e.g. being hovered over by the client.

Basically this:

```
syntax::ast::Root -> (hir::Root, TwoWayPointers, Vec<LowerError>)
```

### `crates/statics`

Does static analysis ("typechecking") on HIR.

The other passes thus far operate on single files, but this one is meant to be run "across" many files. So we take in and give out updated state.

Basically this:

```
(statics::State, hir::Root) -> (statics::State, Vec<StaticsError>)
```

### `crates/analysis`

Unifies all the passes into one single API.

### `crates/lang-srv`

Depends on `analysis` and a bunch of third party crates to actually construct the language server. This is the only binary target, and only this may perform IO.

### `doc`

Documentation, like this!

### `extensions/vscode`

The VS Code client extension, in TypeScript.

[xtask]: https://github.com/matklad/cargo-xtask
[rowan]: https://github.com/rust-analyzer/rowan
[ungrammar]: https://github.com/rust-analyzer/ungrammar
