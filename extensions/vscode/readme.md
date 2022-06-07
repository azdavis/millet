# Millet

[Standard ML][sml] (SML) support for VS Code.

## Features

- For SML and SML/NJ CM files:
  - Syntax highlighting
  - Language configuration (comments, brackets, etc)
  - Snippets
- For SML files only:
  - Inline errors

## Usage

1. Install the extension.
2. Open VS Code to a folder containing a single `.cm` file at the root.
3. Ensure that `.cm` file lists all the SML files in the folder, in the order you wish for them to be analyzed.

### Example

#### `sources.cm`

```sml-nj-cm
Group is
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

## Warning

The [language server][lang-srv] is alpha-quality software. It might be slow, wrong, incomplete, or unstable.

You can turn it off by setting `millet.useLanguageServer` to `false` in your VS Code settings.

[lang-srv]: https://microsoft.github.io/language-server-protocol/
[sml]: https://smlfamily.github.io
