# Manual

This is a manual for how to use Millet.

See also the [blog post](https://azdavis.net/posts/millet/) for an overview.

## A note about VS Code

We provide an official [VS Code][vs-code] extension, but it should be possible to use the compiled language server binary with any editor that supports language servers.

Some features are provided by the VS Code extension, not the language server.

## Setup

### VS Code

Install the extension from the [VS Code marketplace][vs-code-ext].

### Other editors

1. Clone the repository.
2. Build from source with `cargo build --release --bin lang-srv`.
3. Put the compiled `target/release/lang-srv` binary somewhere your editor can find it.
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

For more exotic projects, you may wish to create an optional `millet.toml`.

**Note:** If a file is not transitively reachable from the root group file, it will not be analyzed.

## Configuration

There are two places where Millet can be configured:

- `millet.toml`. This is for project-wide settings.
- VS Code extension settings. This is for user-specific settings.

### `millet.toml`

Millet can be configured with a `millet.toml` in the workspace root. It has the following format:

```toml
version = 1
[workspace]
root = "foo.cm"
[workspace.path-vars]
FOO = { value = "bar" }
QUZ = { path = "lib" }
[errors]
5011.severity = "warning"
4015.severity = "error"
5029.severity = "ignore"
```

- `version` is the version of the config file. At time of writing, it must be exactly `1`.
- `workspace` is configuration for the workspace.
  - `root` sets the root group file. In the case where there is exactly one group file in the root project folder, Millet infers that group file to be the root group file. But if not, it must be explicitly set here.
  - `path-vars` is a table for expanding variables in paths in group files.
    - If the value is a `value`, the value is used unchanged.
    - If it is a `path`, then the value is expanded into a full path relative to the `millet.toml` file.
  - `errors` is a table for configuring error codes.
    - Each key is an error code.
    - For each key, the value is a table.
      - The `severity` key overrides the default severity for this error. The acceptable values are:
        - `ignore`: the error is not reported.
        - `warning`: the error is reported as a warning.
        - `error`: the error is reported with maximum severity.

<!--

Alternatively, it may have the following other, mutually exclusive format:

```toml
version = 1
[workspace]
members = ["*"]
```

The `version = 1` is the same, but the `workspace` table now contains only one key, `members`, which is a list of paths to directories. These paths are each treated as their own "roots", i.e. each path may contain either a root group file or its own `millet.toml`. Each path is analyzed separately from the others.

The string `"*"` is treated specially to mean "every immediate subdirectory of the directory in which `millet.toml` resides". There is no other special treatment of glob-like paths.

A key use case for this special format is e.g. when taking a class taught in Standard ML, in which each assignment is distributed separately from the others over time. A student may:

1. Create a single "class" folder, into which they may place a `millet.toml` file as above.
2. Successively download assignment handouts and place them in the class folder, yet always open VS Code onto this same class folder. Each assignment will be analyzed separately, but there is no need to re-open VS Code to each new assignment.

 -->

### VS Code settings

Millet offers the following configuration options via VS Code settings:

#### `millet.server.enable`

- Type: `boolean`
- Default: `true`

Enable the language server.

#### `millet.server.path`

- Type: `string`
- Default: `""`

Path to the `lang-srv` executable. When set to the empty string `""` (the default), use the path to the one that's pre-built and bundled with the extension.

#### `millet.server.hover.token.enable`

- Type: `boolean`
- Default: `true`

Show information about tokens on hover.

#### `millet.server.diagnostics.onChange.enable`

- Type: `boolean`
- Default: `false`

Send diagnostics when file contents change before saving.

#### `millet.format.enable`

- Type: `boolean`
- Default: `false`

**WARNING: THE FORMATTER REWRITES YOUR CODE. IT IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

Naively format open SML files on save.

## Features

Millet has a bevy of features to help you read, write, and understand SML code.

### (VS Code only) Syntax highlighting

Keywords, literals, comments, etc are highlighted in these files:

| Full name                  | Short name | Extensions             |
| -------------------------- | ---------- | ---------------------- |
| Standard ML                | SML        | `.sml`, `.sig`, `.fun` |
| ML Basis                   | MLB        | `.mlb`                 |
| SML/NJ Compilation Manager | CM         | `.cm`                  |

### (VS Code only) Bracket and comment configuration

All of the above files types also have settings to inform VS Code what the comment delimiters are, and what kinds of brackets should be auto-matched (like `[]`).

This allows things like:

- Use the "toggle comment" keybinding in these files to comment out a line.
- Type e.g. a `{`, and the editor will auto-insert the matching `}`.

### (VS Code only) Snippets

All of the above files have some pre-defined snippets. These can be triggered by typing the "prefix" word and then hitting a "commit character" (like tab).

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
  - `function` dec
  - `structure` dec
  - `signature` dec
  - `functor` dec
  - `println` exp

### Inline errors

Millet will analyze source (SML) and group (MLB/CM) files and report errors directly on the offending area of the file.

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

[cm]: https://www.smlnj.org/doc/CM/new.pdf
[mlb]: http://mlton.org/MLBasis
[vs-code-ext]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[vs-code]: https://code.visualstudio.com
