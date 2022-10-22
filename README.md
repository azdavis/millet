# Millet 💻🦜

A language server for [Standard ML][sml] (SML).

Millet analyzes SML code without running it, and provides information like:

- Inline errors
- Hover for type/documentation
- Jump to definition

Note that Millet does not actually run SML code. To do that, you'll need an installation of SML, like [SML/NJ][smlnj] or [MLton][mlton].

- Check out the [blog post][blog] introducing the project.
- Refer to the [manual][] for information about setup, usage, and features.

## Warning

The [language server][lang-srv] is [beta-quality software][known-issues]. It might be slow, wrong, incomplete, or unstable.

## Install

There is a [VS Code][vscode] extension on the [marketplace][].

VS Code is the only editor for which we provide an "official" extension. However, because Millet is a language server, it should be able to be adapted to work with any editor that supports language servers.

## Community

Millet is affiliated with Project Savanna, a project whose goal is to improve tooling for Standard ML.

There is a [Discord server][discord] for Project Savanna, and Millet has its own channel there. You can get support and discuss the project there.

## Development

Install the dependencies:

- [git][], to clone the repository.
- [rust][] (i.e. `rustc` and `cargo`), to build Rust code.
- [nodejs][] (i.e. `node` and `npm`), to build the VS Code extension in TypeScript.

Then, `git clone` the repo, `cd` inside, and run `cargo xtask ci`.

If you're using VS Code, you can try out the VS Code extension:

1. Open the root directory of this repository in VS Code.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select "extension" in the drop down.
4. Press the green play button.

See also the [documentation][].

## Naming

"Millet" has the letters M and L in it, in that order. So does "Standard ML".

Also:

- Birds eat millet.
- A bird named Polly Morphism is the mascot for [15-150][cmu150], Carnegie Mellon's introductory functional programming course.
- 15-150 is taught in Standard ML.

## License

Millet is dual-licensed under the terms of both the MIT license and the Apache license v2.0.

[blog]: https://azdavis.net/posts/millet/
[cmu150]: http://www.cs.cmu.edu/~15150/
[discord]: https://discord.gg/hgPSUby2Ny
[documentation]: /docs/README.md
[git]: https://git-scm.com
[known-issues]: /docs/known-issues.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[manual]: /docs/manual.md
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[mlton]: http://mlton.org
[nodejs]: https://nodejs.org/en/
[rust]: https://rustup.rs
[sml]: https://smlfamily.github.io
[smlnj]: https://www.smlnj.org
[vscode]: https://code.visualstudio.com
