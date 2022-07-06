# Millet

A language server for [Standard ML][sml].

## Warning

The [language server][lang-srv] is [alpha-quality software][known-issues]. It might be slow, wrong, incomplete, or unstable.

## Installation for [VS Code][vscode]

Install it from the [marketplace][].

## Development

Install the dependencies:

- [git][], to clone the repository and git dependencies
- [rust][], to compile Rust code
- [node][], to build to VS Code extension in TypeScript

Then, `git clone` the repo, `cd` inside, and run `cargo xtask ci`.

If you're using VS Code, you can try out the VS Code extension:

1. Open the root directory of this repository in VS Code.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select "extension" in the drop down.
4. Press the green play button.

See also the [architecture][] documentation.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also:

- Birds eat millet.
- A bird named Polly Morphism is the mascot for [15-150][cmu150], Carnegie Mellon's introductory functional programming course.
- 15-150 is taught in Standard ML.

[architecture]: /docs/architecture.md
[cmu150]: http://www.cs.cmu.edu/~15150/
[git]: https://git-scm.com
[known-issues]: /docs/known-issues.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[node]: https://nodejs.org/en/
[rust]: https://rustup.rs
[sml]: https://smlfamily.github.io
[vscode]: https://code.visualstudio.com
