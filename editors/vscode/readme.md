# Millet

[Standard ML][sml] (SML) support for VS Code.

Millet analyzes SML code without running it, and provides information like:

- Inline errors
- Hover for type/documentation
- Jump to definition

Note that Millet does not actually run SML code. To do that, you'll need an installation of SML, like [SML/NJ][smlnj] or [MLton][mlton].

- Check out the [blog post][blog] introducing the project.
- Refer to the [manual][] for information about setup, usage, and features.

## Warning

The [language server][lang-srv] is [beta-quality software][known-issues]. It might be slow, wrong, incomplete, or unstable.

You can turn it off by setting `millet.server.enable` to `false` in your VS Code settings.

## Usage

1. Install the extension.
2. Open VS Code to a folder directly containing a single "group file", i.e. one of
   - a [ML Basis][mlb] file, with extension `.mlb`
   - a [SML/NJ Compilation Manager][cm] file, with extension `.cm`
3. Ensure that group file lists all the SML/other group files in the folder, in the order you wish for them to be analyzed.

**Note:** If a file is not transitively reachable from the root group file, it will not be analyzed.

### Example

#### `sources.mlb`

```mlb
a.sml
b.sml
```

#### `a.sml`

```sml
(*!
 * `increment x` returns one more than `x`.
 *)
fun increment x = x + 1
```

#### `b.sml`

```sml
val _ = increment 3
```

## Community

Millet is affiliated with Project Savanna, a project whose goal is to improve tooling for Standard ML.

There is a [Discord server][discord] for Project Savanna, and Millet has its own channel there. You can get support and discuss the project there.

## License

Millet is dual-licensed under the terms of both the MIT license and the Apache license v2.0.

[blog]: https://azdavis.net/posts/millet/
[cm]: https://www.smlnj.org/doc/CM/new.pdf
[discord]: https://discord.gg/hgPSUby2Ny
[known-issues]: https://github.com/azdavis/millet/blob/main/docs/known-issues.md
[manual]: https://github.com/azdavis/millet/blob/main/docs/manual.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[mlb]: http://mlton.org/MLBasis
[mlton]: http://mlton.org
[sml]: https://smlfamily.github.io
[smlnj]: https://www.smlnj.org
