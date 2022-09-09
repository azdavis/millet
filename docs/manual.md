# Manual

This is a manual for how to use Millet.

If a feature of Millet is not documented here, please file an issue.

See also the [blog post](https://azdavis.net/posts/millet/) for an overview.

## A note about VS Code

We provide an official VS Code extension, but it should be possible to use the language server alone with any supported editor.

Some features are provided by the VS Code extension, not the language server. These are noted with "(VS Code)".

## (VS Code) Syntax highlighting

Keywords, literals, comments, etc are highlighted in these files:

| Full name                  | Short name | Extensions             |
| -------------------------- | ---------- | ---------------------- |
| Standard ML                | SML        | `.sml`, `.sig`, `.fun` |
| ML Basis                   | MLB        | `.mlb`                 |
| SML/NJ Compilation Manager | CM         | `.cm`                  |

## (VS Code) Bracket and comment configuration

All of the above files types also have settings to inform VS Code what the comment delimiters are, and what kinds of brackets should be auto-matched (like `[]`).

This allows things like:

- Use the "toggle comment" keybinding in these files to comment out a line.
- Type e.g. a `{`, and the editor will auto-insert the matching `}`.

## (VS Code) Snippets

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

## Inline errors

Millet will analyze source (SML) and group (MLB/CM) files and report errors directly on the offending area of the file.

## Hover for info

In SML files, hover over something to get more information on it.

Millet shows things like:

- The type of expressions or patterns.
- Documentation for an item.
- Documentation for tokens.

## Jump/peek definition

In SML files, Millet allows jumping to or peeking the definition of named items, like variables.

## Doc comments

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

## Holes

Millet allows writing `...` or `_` as a "hole" in various contexts (expression, type, declaration, etc) in SML files. They are parsed, but rejected in later stages of analysis.

This allows writing "example" code that actually parses. In the case of expression holes, the error message also reports the inferred type of the hole.

## Code actions

Millet provides some code actions to edit the source code.

### Fill case

When your cursor is over the `case` or `of` keywords of a `case` expression, Millet can fill in the case with arms for each variant of the type of the head expression.
