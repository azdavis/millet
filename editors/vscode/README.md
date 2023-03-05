# Millet

[Standard ML][sml] (SML) support for VS Code.

Millet analyzes SML code without running it, and provides information like:

- Syntax highlighting
- Bracket and comment configuration
- Snippets
- Inline diagnostics
- Hover for type/documentation
- Jump to definition
- Document symbols
- Find all references

Note that Millet does not actually run SML code. To do that, you'll need an installation of SML, like [SML/NJ][smlnj] or [MLton][mlton].

- Check out the [blog post][blog] introducing the project.
- Refer to the [manual][] for information about setup, configuration, and features.
- See the full [documentation][] for other information, like explanations for diagnostics and project policies.

## Usage

First, install the extension, and open VS Code on a folder with SML files.

If VS Code does not automatically detect how to analyze the project, add a `millet.toml` that defines the "root" of the project. All files should be reachable from this root.

To define the root, set `workspace.root` in `millet.toml` to the path to a file that lists out all the SML files Millet should analyze. This file should be either:

- a [ML Basis][mlb] (MLB) file, with extension `.mlb`
- a [SML/NJ Compilation Manager][cm] (CM) file, with extension `.cm`

MLB and CM are the two most common ways of managing large SML projects with many files.

In the example below, we've opened VS Code to a folder with 4 files: a Millet config file, a "root" file with MLB syntax, and two SML files.

### `millet.toml`

```toml
version = 1
workspace.root = "sources.mlb"
```

### `sources.mlb`

```mlb
a.sml
b.sml
```

### `a.sml`

```sml
(*!
 * `increment x` returns one more than `x`.
 *)
fun increment x = x + 1
```

### `b.sml`

```sml
val four = increment 3
val () = "if you see a type error, Millet is working!"
```

## Community

Millet is affiliated with Project Savanna, a project whose goal is to improve tooling for Standard ML.

There is a [Discord server][discord] for Project Savanna, in which Millet has its own channel. You can get support and discuss the project there.

## License

Millet is dual-licensed under the terms of both the MIT license and the Apache license v2.0.

[blog]: https://azdavis.net/posts/millet/
[cm]: https://www.smlnj.org/doc/CM/new.pdf
[discord]: https://discord.gg/hgPSUby2Ny
[documentation]: https://github.com/azdavis/millet/blob/main/docs/README.md
[manual]: https://github.com/azdavis/millet/blob/main/docs/manual.md
[mlb]: http://mlton.org/MLBasis
[mlton]: http://mlton.org
[sml]: https://smlfamily.github.io
[smlnj]: https://www.smlnj.org
