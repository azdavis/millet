# Manual

This is a manual for how to use Millet.

See also the [blog post](https://azdavis.net/posts/millet/) for an overview.

## A note about VS Code

We provide an official [VS Code][vs-code] extension, but it should be possible to use the compiled language server binary with any editor that supports language servers.

Some features are provided by the VS Code extension, not the language server.

Note also that when we say "VS Code", we really mean "VS Code or any similar compatible editor". VS Code is technically Microsoft's custom distribution of the open-source "Code - OSS" project, upon which things like [VSCodium][] are built.

Only Microsoft's VS Code is allowed to use the extension marketplace, so Millet is also available on [Open VSX][ovsx] for VS Code-compatible editors like VSCodium.

## Setup

### VS Code

Install the extension from the [VS Code marketplace][marketplace].

### Other editors

1. Clone or download the repository.
2. Build from source with `cargo build --release --bin millet-ls`.
3. Put the compiled `target/release/millet-ls` binary somewhere your editor can find it.
4. Set up your editor to use that binary to process SML files.

## Usage

### On a file

When VS Code is not opened onto a folder, Millet only provides basic analysis of individual SML files. For project-wide analysis and access to the most features, open VS Code onto a folder instead.

### On a folder

After opening VS Code onto a folder, Millet will look for a "group file" directly contained in that folder.

A group file is either

- a [ML Basis][mlb] file (`.mlb`), or
- a [SML/NJ Compilation Manager][cm] file (`.cm`).

These files list out SML source files and other group files to organize the project.

For more exotic projects, you may wish to create an optional [`millet.toml`](#millettoml).

Note that a group file, or a `millet.toml` file pointing to a group file, **must** be present **directly** in the directory that you open VS Code onto. It can't be in subdirectories, because Millet will not look in subdirectories, unless you tell it to via `millet.toml`.

If a file is not transitively reachable from the root group file, it **will not** be analyzed.

## Configuration

There are four places where Millet can be configured:

- [`millet.toml`](#millettoml). This is for project-wide settings.
- [VS Code settings](#vs-code-settings). This is for user-specific settings.
- [Language server initialization](#language-server-initialization). This is for advanced use-cases and/or non-VS-Code editors.
- [ML Basis annotations](#ml-basis-annotations). This is for specific files.

### `millet.toml`

Millet can be configured with a `millet.toml` in the workspace root. It is a [TOML][] file with the following format:

```toml
version = 1
[workspace]
root = "foo.cm"
[workspace.path-vars]
FOO = { value = "woof" }
BAR = { path = "bork" }
QUZ = { workspace-path = "pant" }
[diagnostics]
5011.severity = "warning"
4015.severity = "error"
5029.severity = "ignore"
[language]
fixity-across-files = true
[language.exp]
while = false
[language.dec]
functor = true
[language.val]
"=" = false
[language.structure]
"Ref" = false
[language.successor-ml]
or-pat = false
```

#### `version`

The version of the config file. At time of writing, it must be exactly `1`.

#### `workspace`

Configuration for the workspace.

#### `workspace.root`

Sets the root group file(s).

In the case where there is exactly one group file in the root project folder, Millet infers that group file to be the root group file. But if not, it must be explicitly set here.

You can use glob syntax for this to specify multiple roots.

#### `workspace.path-vars`

A table for expanding variables in paths in group files.

- If the value is a `value`, the value is used unchanged.
- If it is a `path`, then the value is expanded into a full path relative to the `millet.toml` file.
- If it is a `workspace-path`, then the value is expanded into a full path relative to the workspace root group file (of which there may be many, because `workspace.root` can be a glob).

#### `diagnostics`

A table for configuring diagnostic codes.

#### `diagnostics.<code>`

Configuration for the diagnostic with code number `<code>`. It must be a positive integer.

#### `diagnostics.<code>.severity`

Overrides the default severity for this [diagnostic](#inline-diagnostics). The acceptable values are:

- `"ignore"`: the diagnostic is not reported.
- `"warning"`: the diagnostic is reported as a warning.
- `"error"`: the diagnostic is reported as an error.

#### `language`

Configuration for the language.

#### `language.fixity-across-files`

Whether fixity declarations (`infix`, `infixr`, and `nonfix`) can take effect across files. Defaults to `false`.

When this is `false`, each file is parsed starting with the default fixity environment provided by the standard basis. This means we can incrementally re-parse files and/or parse files in parallel since there are no inter-file dependencies when parsing. (At time of writing, we do not currently do this.)

When this is `true`, we cannot do the above things, and we must also use more memory to store the fixity environments used when parsing each file.

#### `language.dec`

What kinds of declarations/specifications are permitted.

#### `language.dec.<kind>`

Whether the `<kind>` of declaration/specification is allowed. Each defaults to `true`.

Valid `<kind>`s:

- `val`
- `fun`
- `type`
- `datatype`
- `datatype-copy`
- `exception`
- `open`
- `fixity` (covers `infix`, `infixr`, and `nonfix`)
- `local`
- `structure`
- `signature`
- `functor`
- `exp` (expression declarations)
- `include` (technically a specification, not a declaration)

#### `language.exp`

What kind of expressions are permitted.

#### `language.exp.<kind>`

Whether the `<kind>` of expression is allowed. Each defaults to `true`.

Valid `<kind>`s:

- `int-lit`
- `real-lit`
- `word-lit`
- `char-lit`
- `string-lit`
- `path`
- `record`
- `selector`
- `paren`
- `tuple`
- `list`
- `seq`
- `let`
- `app`
- `infix`
- `typed`
- `andalso`
- `orelse`
- `handle`
- `raise`
- `if`
- `while`
- `case`
- `fn`

#### `language.val`

Configuration for values.

#### `language.val.<path>`

Whether the `<path>` is allowed. All paths default to `true`.

The path must be a valid, fully qualified path in the standard basis library, like `Real.==` or `+`.

Because paths can have special characters in them, namely `.`, you may need to use TOML's quoted path syntax. Like this:

```toml
[language.val]
"List.tabulate" = false
```

Note that some standard basis library declarations are re-declared at different paths. To disallow them entirely, you must (currently) specify all possible paths. For instance, you should specify both `hd` and `List.hd` to disallow usage of the list head function.

No error is currently emitted when disallowing a path that does not exist. To be sure you spelled the path correctly, try running Millet with the given config file, and see if errors are correctly emitted when you try to use the value.

#### `language.structure`

Configuration for structures.

#### `language.structure.<path>`

Whether the `<path>` is allowed. All paths default to `true`.

See docs for [`language.val.<path>`](#languagevalpath).

#### `language.successor-ml`

Configuration for [Successor ML][succ-ml] features.

#### `language.successor-ml.<kind>`

Whether the `<kind>` is allowed.

| Valid `<kind>` | Default | Description                                |
| -------------- | ------- | ------------------------------------------ |
| `or-pat`       | `true`  | Or patterns                                |
| `do-dec`       | `false` | `do` declarations                          |
| `opt-bar`      | `false` | Optional preceding vertical bars           |
| `opt-semi`     | `false` | Optional trailing `;` in `let` expressions |
| `exp-row-pun`  | `false` | Record expression row punning              |

### VS Code settings

Millet has VS Code specific settings, which are stored as [JSON][]. You may need to reload VS Code and/or Millet to pick up the changes.

<!-- @begin vscode-config -->

#### `millet.format.engine`

**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

How to [format](#formatter) open SML files on save.

- Type: `string`
- Default: `"none"`
- Valid values:
  - `"none"`: No formatting.
  - `"naive"`: Naive formatting.
  - `"smlfmt"`: Formatting provided by [`smlfmt`](https://github.com/shwestrick/smlfmt), which must be installed in your `$PATH`.

#### `millet.server.diagnostics.ignore`

What [diagnostics](#inline-diagnostics) to ignore.

- Type: `string`
- Default: `"after-syntax"`
- Valid values:
  - `"none"`: Ignore no diagnostics, i.e. send all diagnostics.
  - `"after-syntax"`: If there are syntax diagnostics (lex error, parse error, etc), send only those, and ignore e.g. statics diagnostics.
  - `"all"`: Ignore all diagnostics.

#### `millet.server.diagnostics.moreInfoHint.enable`

Show a hint on diagnostic messages about clicking the error code number for more information.

- Type: `boolean`
- Default: `true`

#### `millet.server.diagnostics.onChange.enable`

Send diagnostics when file contents change before saving.

- Type: `boolean`
- Default: `false`

#### `millet.server.enable`

Enable the language server.

- Type: `boolean`
- Default: `true`

#### `millet.server.fileSystemWatcher.enable`

Use a file system watcher to send events when files change, if one is available.

- Type: `boolean`
- Default: `true`

#### `millet.server.hover.token.enable`

Show information about tokens on hover.

- Type: `boolean`
- Default: `true`

#### `millet.server.path`

Path to the `millet-ls` executable.

When set to the empty string `""` (the default), use the path to the one that's pre-built and bundled with the extension.

- Type: `string`
- Default: `""`

<!-- @end vscode-config -->

### Language server initialization

If you're using VS Code, the VS Code extension automatically passes the appropriate required custom initialization options to the language server process when starting it up.

If you're not using VS Code, you will have to arrange to pass these initialization options through some other editor-specific means.

The initialization options are a subset of the VS Code config, but rearranged and renamed slightly. Consult the implementation of the VS Code extension to see what options are sent. Additionally, consult the documentation for the VS Code configuration to see what types the configuration options must be.

### ML Basis annotations

Millet knows about some [ML Basis annotations][mlb-ann]. The ones not mentioned here are ignored.

#### `milletDiagnosticsIgnore`

How to ignore [diagnostics](#inline-diagnostics) for the files in the annotated basis declaration.

Possible arguments:

- `all`: Ignore all diagnostics.

For example, suppose we have 3 files, each quite similar, and each containing a type error:

| Filename | Contents       |
| -------- | -------------- |
| `a.sml`  | `val () = "a"` |
| `b.sml`  | `val () = "b"` |
| `c.sml`  | `val () = "c"` |

Even though each file would normally emit a type error, given the following root ML Basis file, the errors are reported in `a.sml` and `c.sml` only, and not `b.sml`:

```text
a.sml
ann "milletDiagnosticsIgnore all" in
  b.sml
end
c.sml
```

## Features

Millet has a bevy of features to help you read, write, and understand SML code.

### (VS Code only) Syntax highlighting

VS Code will highlight keywords, literals, comments, etc in these files:

| Full name                  | Short name | Extensions             |
| -------------------------- | ---------- | ---------------------- |
| Standard ML                | SML        | `.sml`, `.sig`, `.fun` |
| ML Basis                   | MLB        | `.mlb`                 |
| SML/NJ Compilation Manager | CM         | `.cm`                  |

### (VS Code only) Bracket and comment configuration

VS Code knows about things like comment delimiters and what kinds of brackets should be auto-matched in these files.

This allows for features like:

- Use the "toggle comment" keybinding in these files to comment out a line.
- Type e.g. a `{`, and the editor will auto-insert the matching `}`.

### (VS Code only) Snippets

In VS Code, all of the above files have some pre-defined snippets. These can be triggered by typing the "prefix" word and then hitting a "commit character" (like tab).

#### ML Basis

<!-- @begin mlb-snippets -->

- `let` exp
- `bas` exp
- `local` dec
- `ann` dec

<!-- @end mlb-snippets -->

#### SML/NJ CM

<!-- @begin sml-nj-cm-snippets -->

- `group` desc
- `library` desc

<!-- @end sml-nj-cm-snippets -->

#### SML

<!-- @begin sml-snippets -->

- `let` exp
- `case` exp
- `if` exp
- `handle` exp tail
- `local` dec
- `datatype` dec
- `fun` dec
- `structure` dec
- `signature` dec
- `functor` dec
- `print` with newline exp

<!-- @end sml-snippets -->

### Inline diagnostics

Millet will analyze source (SML) and group (MLB/CM) files and report diagnostics directly on the offending area of the file.

Each diagnostics has a default severity, e.g. "error" or "warning". This can be overridden with [`diagnostics.<code>.severity`](#diagnosticscodeseverity) in `millet.toml`. A severity of `"ignore"` ignores (i.e. disables) the diagnostic.

Diagnostics can be ignored for an entire file or set of files with the [`milletDiagnosticsIgnore`](#milletdiagnosticsignore) ML Basis annotation. Use:

```
ann "milletDiagnosticsIgnore all" in
  foo.sml
  bar.sml
end
```

Diagnostics can be ignored for all files with the [`millet.server.diagnostics.ignore`](#milletserverdiagnosticsignore) VS Code setting.

### Hover for info

In SML files, hover over something to get more information on it.

Millet shows things like:

- The type of expressions or patterns.
- Documentation for an item.
- Documentation for tokens.

See the section on [doc comments](#doc-comments) to provide your own documentation for items.

### Inlay hints

In SML files, Millet can show inlay hints with type annotations.

In VS Code, inlay hints can be enabled or disabled across the entire editor via `editor.inlayHints.enabled`.

### Jump/peek definition

In SML files, Millet allows jumping to or peeking the definition of named items, like variables.

### Completions

Millet provides completions for the current cursor location. Completions can be triggered by typing a regular name or `.` after a name. When typing `.`, Millet will traverse the existing path.

In this example, Millet provides the given completions at the given cursor location.

```sml
structure Foo = struct
  val bar = 3
  val quz = "hi"
end

val _ = Foo.
(**         ^ completions: bar, quz *)
```

### Code action: fill case

When your cursor is over the `case` or `of` keywords of a `case` expression, Millet can fill in the case with arms for each variant of the type of the head expression.

### Document symbols

Millet can show all the symbols in a document, and information about those symbols.

### Find all references

Millet supports finding references to a symbol.

### Doc comments

Millet allows defining documentation comments on items to be shown on hover.

Use this comment style:

```sml
(*!
 * `inc x` returns one more than `x`.
 *)
fun inc x = x + 1
```

So, put `(*!` on its own line, then the doc comment in Markdown with leading `*` on each line, and then `*)` on its own line.

### Holes

Millet allows writing `...` or `_` as a "hole" in various contexts (expression, type, declaration, etc) in SML files. They are parsed, but rejected in later stages of analysis.

This allows writing "example" code that actually parses. In the case of expression holes, the error message also reports the inferred type of the hole.

### Formatter

**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

Millet can automatically format your open SML files. Set `millet.format.engine` to something other than `"none"` in your settings, then reload your editor. Now, when saving an open file, Millet will format.

- If you use `"naive"`, the formatter is built-in to Millet.
- If you use `"smlfmt"`, you'll need to install [`smlfmt`][smlfmt] in your `PATH`.

There are some other caveats.

#### Experimental

The first and most obvious caveat is the one above, in big bold uppercase.

Most Millet features only **read** your files, and produce output only in the form of things like

- diagnostics
- hover information
- locations for definition sites

If Millet has a bug, then the worst that happens is it provides incorrect information about your files. This can be safely ignored with no damage to your files.

The formatter, however, **rewrites** your files. If there is a bug in the formatter, in the worst case, it could overwrite your files with

- invalid syntax
- garbage
- nothing at all (i.e. essentially delete your files)

So, the formatter is disabled by default, and great care should be taken when using it.

#### Long lines

The naive Millet formatter employs exceedingly unsophisticated strategies to break code across many lines. What this means is that large expressions (e.g. a function call expression with many long arguments) may be formatted all on one line.

The suggested workaround is to use a `let ... in ... end` expression and split out sub-expressions into variables. So instead of:

```sml
Boop.beep (if bar x then quz y else Fee.Fi.Fo.fum z) (fn res => s (blab :: res)) (fn () => k []) (fn (x, ac) => ac andalso x) (xs @ ys @ zs)
```

Try something like:

```sml
let
  val fst = if bar x then quz y else Fee.Fi.Fo.fum z
  fun succeed res = s (blab :: res)
  fun fail () = k []
  fun andBoth (x, ac) = ac andalso x
  val all = xs @ ys @ zs
in
  Boop.beep fst succeed fail andBoth all
end
```

An arguably good thing about this is that it might improve readability anyway.

#### Comments

The naive formatter completely gives up on formatting the file if a comment appears in a place that the formatter doesn't know how to deal with.

The **only** kind of comment the formatter even **attempts** to deal with are comments directly above declarations, like these:

```sml
(* a is the first letter *)
val a = 1

(* id x returns x unchanged *)
fun id x = x

(* isomorphic to bool *)
datatype dayKind = Weekday | Weekend
```

Comments in other positions, like inside expressions, are not supported:

```sml
val uh =
  if foo then
    (* always bar when foo *)
    bar
  else
    (* fall back to quz otherwise *)
    quz
```

When the formatter cannot format a file, it simply does nothing, and the file will not be formatted. It emits warnings pointing at the comments that prevented formatting.

#### Configuration

There are no options to configure the formatter.

This is by design. More options means more [bikeshedding][bike-shed].

The formatter does, however, respect the editor-configured tab size.

[cm]: https://www.smlnj.org/doc/CM/new.pdf
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[mlb]: http://mlton.org/MLBasis
[ovsx]: https://open-vsx.org/extension/azdavis/millet
[vs-code]: https://code.visualstudio.com
[vscodium]: https://vscodium.com
[toml]: https://toml.io/en/
[json]: https://www.json.org/json-en.html
[smlfmt]: https://github.com/shwestrick/smlfmt
[mlb-ann]: http://mlton.org/MLBasisAnnotations
[bike-shed]: https://en.wikipedia.org/wiki/Law_of_triviality
[succ-ml]: http://mlton.org/SuccessorML
