# Millet

[Standard ML][sml] (SML) support for VS Code.

See the [blog post][blog] introducing the project.

Millet analyzes SML code without running it, and provides information like:

- Inline errors
- Hover for type/documentation
- Jump to definition

Note that Millet does not actually run SML code. To do that, you'll need an installation of SML, like [SML/NJ][smlnj] or [MLton][mlton].

## Warning

The [language server][lang-srv] is [beta-quality software][known-issues]. It might be slow, wrong, incomplete, or unstable.

You can turn it off by setting `millet.useLanguageServer` to `false` in your VS Code settings.

## Features

- Syntax highlighting
- Language configuration (comments, brackets, etc)
- Snippets
- Inline errors
- Hover for type/documentation
- Go to definition/type definition
- Code actions, like "fill case"

## Usage

1. Install the extension.
2. Open VS Code to a folder containing a single "group file", i.e. one of
   - a ML Basis file, with extension `.mlb`
   - a SML/NJ CM file, with extension `.cm`
3. Ensure that group file lists all the SML/other group files in the folder, in the order you wish for them to be analyzed.

**Note:** If a file is not transitively reachable from the root group file, it will not be analyzed.

### Example

#### `sources.mlb`

```mlb
Foo.sml
Bar.sml
```

#### `Foo.sml`

```sml
structure Foo = struct
  fun fact 0 = 1
    | fact n = n * fact (n - 1)
end
```

#### `Bar.sml`

```sml
val _ = Foo.fact 3
```

[blog]: https://azdavis.net/posts/millet/
[known-issues]: https://github.com/azdavis/millet/blob/main/docs/known-issues.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[mlton]: http://mlton.org
[sml]: https://smlfamily.github.io
[smlnj]: https://www.smlnj.org
