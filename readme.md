# Millet

Tools for [Standard ML][sml], most notably a [language server][lang-srv].

## Warning

The language server is alpha-quality software. It might be slow, wrong, [incomplete][], or unstable.

## Installation for [VS Code][vscode]

Install it from the [marketplace][].

## Development

Install the dependencies: git, rust, and node.

Then, `git clone` the repo, `cd` inside, and run `cargo xtask ci`.

If you're using VS Code, you can try out the VS Code extension:

1. Open the root directory of this repository in VS Code.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select 'extension' in the drop down.
4. Press the green play button.

See also the [architecture][] documentation.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for [15-150][cmu150], Carnegie Mellon's introductory functional programming course taught in Standard ML.

[architecture]: doc/architecture.md
[cmu150]: http://www.cs.cmu.edu/~15150/
[incomplete]: doc/todo.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[node]: https://nodejs.org/en/
[rustup]: https://rustup.rs
[sml]: https://smlfamily.github.io
[vscode]: https://code.visualstudio.com
