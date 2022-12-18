# Millet

A language server for [Standard ML][sml] (SML).

![Millet logo](./editors/vscode/icon.png)

Millet analyzes SML code without running it, and provides information like:

- Inline diagnostics
- Hover for type/documentation
- Jump to definition
- Document symbols
- Find all references

Note that Millet does not actually run SML code. To do that, you'll need an installation of SML, like [SML/NJ][smlnj] or [MLton][mlton].

- Check out the [blog post][blog] introducing the project.
- Refer to the [manual][] for information about setup, configuration, and features.
- See the full [documentation][] for other information, like explanations for diagnostics and project policies.

## Install

- There is a [VS Code][vscode] extension on the [marketplace][].
- The same extension is also published on [Open VSX][ovsx].

VS Code and compatible editors like [VSCodium][] are the only editors for which we provide an "official" extension. However, because Millet is a language server, it should be able to be adapted to work with any editor that supports language servers.

## Community

Millet is affiliated with Project Savanna, a project whose goal is to improve tooling for Standard ML.

There is a [Discord server][discord] for Project Savanna, and Millet has its own channel there. You can get support and discuss the project there.

## Contributing

We encourage contributions of all kinds. Please read the [contributing][] guide.

## Development

The project is mostly written in Rust, so you will need a Rust installation. If you want to build the VS Code extension in TypeScript as well, you should also install Node.js.

| What            | Version       | Why                   |
| --------------- | ------------- | --------------------- |
| [Rust][rust]    | Latest stable | Build Rust code       |
| [Node.js][node] | 18            | Build TypeScript code |

After installing the dependencies, download/clone the repo and run `cargo xtask ci` inside it. This will build the project and run tests.

If you're using VS Code, you can try out the VS Code extension:

1. Open the root directory of the repo in VS Code.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select "extension" in the drop down.
4. Press the green play button.

## Naming

"Millet" has the letters M and L in it, in that order. So does "Standard ML".

Also:

- Birds eat millet.
- A bird named Polly Morphism is the mascot for [15-150][cmu150], Carnegie Mellon's introductory functional programming course.
- 15-150 is taught in Standard ML.

Note also that Polly is featured in the logo, which was drawn by [Yixin He][yixin].

## License

Millet is dual-licensed under the terms of both the MIT license and the Apache license v2.0.

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
