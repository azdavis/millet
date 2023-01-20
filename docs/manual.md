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

1. Clone the repository.
2. Build from source with `cargo build --release --bin millet-ls`.
3. Put the compiled `target/release/millet-ls` binary somewhere your editor can find it.
4. Set up your editor to use that binary to process SML files.

## Usage

### On a file

When VS Code is not opened onto a folder, Millet analyzes each SML file open in the editor in isolation.

### On a folder

Most of the time, you'll probably want to use Millet to analyze an entire SML project. After opening VS Code onto a folder containing a project, Millet will look for a "group file" directly contained that folder.

A "group file" is either

- a [ML Basis][mlb] file (`.mlb`), or
- a [SML/NJ Compilation Manager][cm] file (`.cm`).

These file types list out SML source files and other group files to organize the project.

For more exotic projects, you may wish to create an optional `millet.toml`. Read on to see how that works.

Some important notes:

- A group file, or a `millet.toml` file pointing to a group file, **must** be present **directly** in the directory that you open VS Code onto. It can't be in subdirectories, because Millet will not look in subdirectories, unless you tell it to via `millet.toml`.
- If a file is not transitively reachable from the root group file, it **will not** be analyzed.

## Configuration

There are two places where Millet can be configured:

- `millet.toml`. This is for project-wide settings.
- VS Code extension settings. This is for user-specific settings.

### Project-wide settings

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

Overrides the default severity for this diagnostic. The acceptable values are:

- `"ignore"`: the diagnostic is not reported.
- `"warning"`: the diagnostic is reported as a warning.
- `"error"`: the diagnostic is reported as an error.

### VS Code settings

Millet offers the following configuration options via VS Code settings, which are stored as [JSON][]:

<!-- @begin vscode-config -->

#### `millet.format.engine`

**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

How to format open SML files on save.

- Type: `string`
- Default: `"none"`
- Valid values:
  - `"none"`: No formatting.
  - `"naive"`: Naive formatting.
  - `"smlfmt"`: Formatting provided by [`smlfmt`](https://github.com/shwestrick/smlfmt).

#### `millet.server.diagnostics.filter`

What diagnostics to send per file.

- Type: `string`
- Default: `"syntax"`
- Valid values:
  - `"none"`: No filter, i.e. all available diagnostics are sent.
  - `"syntax"`: If there are syntax errors (lex, parse, etc), send only those, and do not send e.g. statics diagnostics.

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

The snippets provided are:

- MLB
  - `let` exp
  - `bas` exp
  - `local` dec
- CM
  - `group` desc
  - `library` desc
- SML
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
  - `println` exp

### Inline diagnostics

Millet will analyze source (SML) and group (MLB/CM) files and report diagnostics directly on the offending area of the file.

### Hover for info

In SML files, hover over something to get more information on it.

Millet shows things like:

- The type of expressions or patterns.
- Documentation for an item.
- Documentation for tokens.

### Jump/peek definition

In SML files, Millet allows jumping to or peeking the definition of named items, like variables.

### Doc comments

Related to the "hover" feature, Millet allows defining doc comments on items to be shown on hover.

Use this comment style:

```sml
structure Math = struct
  (*!
   * `inc x` increments the given number.
   *)
  fun inc x = x + 1
end
```

So, put `(*!` on its own line, then the doc comment in Markdown with leading `*` on each line, and then `*)` on its own line.

### Holes

Millet allows writing `...` or `_` as a "hole" in various contexts (expression, type, declaration, etc) in SML files. They are parsed, but rejected in later stages of analysis.

This allows writing "example" code that actually parses. In the case of expression holes, the error message also reports the inferred type of the hole.

### Code action: fill case

When your cursor is over the `case` or `of` keywords of a `case` expression, Millet can fill in the case with arms for each variant of the type of the head expression.

### Document symbols

Millet can show all the symbols in a document, and information about those symbols.

### Find all references

Millet supports finding references to a symbol.

### Formatter

**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

Millet can automatically format your open SML files. Set `millet.format.engine` to something other than `"none"` in your settings, then reload your editor. Now, when saving an open file, Millet will format.

There are some caveats, however.

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

The suggested workaround is to use a `let ... in ... end` expression and split out sub-expressions into variables. So instead of

```sml
Boop.beep (if bar x then quz y else Fee.Fi.Fo.fum z) (fn res => s (blab :: res)) (fn () => k []) (fn (x, ac) => ac andalso x) (xs @ ys @ zs)
```

Try something like

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

The **only** kind of comment the formatter even **attempts** to deal with are comments above declarations, like this:

```sml
(* i'm above a declaration *)
val a = 1
(* and so am i *)
fun id x = x
(* and i as well *)
datatype dayKind = Weekday | Weekend
```

Comments in other positions, like inside expressions, are not supported.

```sml
val uh =
  if foo then
    (* always bar when foo *)
    bar
  else
    (* fall back to quz otherwise *)
    quz
```

When the formatter cannot format a file, it simply does nothing, and the file will not be formatted.

#### Configuration

There are no options to configure the naive formatter (other than to enable it or not).

This is by design. More options means more ability to have different formatting styles.

[cm]: https://www.smlnj.org/doc/CM/new.pdf
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[mlb]: http://mlton.org/MLBasis
[ovsx]: https://open-vsx.org/extension/azdavis/millet
[vs-code]: https://code.visualstudio.com
[vscodium]: https://vscodium.com
[toml]: https://toml.io/en/
[json]: https://www.json.org/json-en.html
