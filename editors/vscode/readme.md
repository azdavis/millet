# Millet

[Standard ML][sml] (SML) support for VS Code.

## Warning

The [language server][lang-srv] is [alpha-quality software][known-issues]. It might be slow, wrong, incomplete, or unstable.

You can turn it off by setting `millet.useLanguageServer` to `false` in your VS Code settings.

## Features

- Syntax highlighting
- Language configuration (comments, brackets, etc)
- Snippets
- Inline errors
- Hover for type/documentation
- Go to definition/type definition

## Usage

1. Install the extension.
2. Open VS Code to a folder containing a single SML/NJ CM file (with extension `.cm`).
3. Ensure that CM file lists all the SML/other CM files in the folder, in the order you wish for them to be analyzed. **If a file is not transitively reachable from the root `sources.cm`, it will not be analyzed.**

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

[known-issues]: https://github.com/azdavis/millet/blob/main/docs/known-issues.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[sml]: https://smlfamily.github.io
