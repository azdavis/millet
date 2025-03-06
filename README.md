# Millet

A [language server][lang-srv] for [Standard ML][sml] (SML).

**Install for: [VS Code][marketplace] • [Open VSX][ovsx]**

![Millet logo](./editors/vscode/icon.png)

Millet analyzes SML code without running it, and provides information like:

- Inline diagnostics
- Hover for type/documentation
- Inlay hints
- Jump to definition
- Code completions
- Code actions
- Document symbols
- Find all references

Millet also supports SML/NJ Compilation Manager (CM) and ML Basis (MLB), allowing for analysis of multi-file SML projects.

Note that Millet does not actually run SML code. To do that, you'll need an installation of SML, like [SML/NJ][smlnj] or [MLton][mlton].

- Check out the [blog post][blog] introducing the project.
- Refer to the [manual][] for information about setup, configuration, and features.
- See the full [documentation][] for other information, like explanations for diagnostics and project policies.

## Install

### Official sources

- There is a [VS Code][vscode] extension on the [marketplace][].
- The same extension is also published on [Open VSX][ovsx].
- Pre-built binaries are on the [releases page][rel].

VS Code and compatible editors like [VSCodium][] are the only editors for which we provide an "official" extension. However, because Millet is a language server, it should be able to be adapted to work with any editor that supports language servers.

### Unofficial sources

Millet is also available in various other package managers and repositories. These are maintained by the community and are **not** official, so they may be out of date.

- [AUR][]
- [Homebrew][]
- [Nixpkgs][]

There is some support for Millet in various editors as well:

- [Emacs](https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=477eb882b57b3defd43ea8dd9510cfdf5fd9ee79)
- [Emacs LSP](https://github.com/emacs-lsp/lsp-mode)
- [Neovim](https://github.com/neovim/nvim-lspconfig)

## Community

Millet is affiliated with Project Savanna, a project whose goal is to improve tooling for Standard ML.

There is a [Discord server][discord] for Project Savanna. The server has a channel for Millet support and discussion.

## Contributing

We encourage contributions of all kinds. Please read the [contributing][] guide.

## Development

### Dependencies

| What            | Version       | Why                   | Required |
| --------------- | ------------- | --------------------- | -------- |
| [Rust][rust]    | Latest stable | Build Rust code       | Yes      |
| [Node.js][node] | 20            | Build TypeScript code | No       |

The project is mostly written in Rust, so you will need a Rust installation. It should include:

- `rustc`, a Rust compiler
- `cargo`, a Rust package manager and build system
- `rustfmt`, a Rust formatter
- `clippy`, a Rust linter

The minimum supported Rust version (MSRV) is the latest stable.

If you want to build the VS Code extension in TypeScript as well, you should also install Node.js.

If your editor supports it, we also recommend installing [Rust Analyzer][ra], a language server for Rust.

### Building/testing

This is a regular Rust project, so the usual Cargo commands will work:

```sh
$ cargo build
$ cargo test
```

We also use clippy and rustfmt, which can be run with cargo as well:

```sh
$ cargo fmt
$ cargo clippy
```

These can all be run together as they are in CI:

```sh
$ cargo xtask ci
```

### VS Code

If you're using VS Code, you can try out the VS Code extension:

1. Open the repository in VS Code.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select "extension" in the drop down.
4. Press the green play button.

## Naming

- "Millet" has the letters M and L in it, in that order. So does "Standard ML".
- Some birds eat millet. A bird named Polly Morphism is the mascot for [15-150][cmu150], Carnegie Mellon's introductory functional programming course, which is taught in Standard ML.

## Logo

The logo features Polly Morphism, wearing a 15-150 style T-shirt, writing SML on a Millet-branded laptop, encircled with millet. [Yixin He][yixin] drew the logo.

## License

Like Rust itself and many other projects in the Rust community, Millet is licensed under either the MIT license or the Apache license v2.0, at your option.

[blog]: https://azdavis.net/posts/millet/
[cmu150]: http://www.cs.cmu.edu/~15150/
[contributing]: /docs/CONTRIBUTING.md
[discord]: https://discord.gg/hgPSUby2Ny
[documentation]: /docs/README.md
[manual]: /docs/manual.md
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[mlton]: http://mlton.org
[node]: https://nodejs.org/en/
[ovsx]: https://open-vsx.org/extension/azdavis/millet
[rust]: https://rustup.rs
[sml]: https://smlfamily.github.io
[smlnj]: https://www.smlnj.org
[vscode]: https://code.visualstudio.com
[vscodium]: https://vscodium.com
[yixin]: https://yixinhe.me
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[ra]: https://rust-analyzer.github.io
[rel]: https://github.com/azdavis/millet/releases/
[aur]: https://aur.archlinux.org/packages/millet
[nixpkgs]: https://search.nixos.org/packages?channel=unstable&show=millet
[homebrew]: https://formulae.brew.sh/formula/millet#default
