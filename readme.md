# Millet

A [language server][lang-srv] for [Standard ML][sml].

Also a corresponding [Visual Studio Code][vscode] extension, available for download on the [marketplace][].

## Warning

The language server is alpha-quality software. It might be slow, wrong, [incomplete][], or unstable.

## Development

`git clone` the repo, `cd` inside, and run `cargo xtask ci`.

If you're using VS Code, you can try out the VS Code extension:

1. Open the root directory of this repository in VS Code.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select 'extension' in the drop down.
4. Press the green play button.

## Repository layout

- Most of the code is in `crates`.
- Editor extensions are in `extensions`.
- The "build system" is in `xtask`. It requires rust, node, and git.

## Testing

Most tests are in `crates/tests` and use `check()` like this:

```rs
use crate::check::check;

#[test]
fn undefined() {
  check(
    r#"
val _ = nope
(**     ^^^^ undefined value: nope *)
"#,
  );
}
```

See the documentation of `check()` for how to write tests.

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for [15-150][cmu150], Carnegie Mellon's introductory functional programming course taught in Standard ML.

[cmu150]: http://www.cs.cmu.edu/~15150/
[incomplete]: doc/todo.md
[lang-srv]: https://microsoft.github.io/language-server-protocol/
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[node]: https://nodejs.org/en/
[rustup]: https://rustup.rs
[sml]: https://smlfamily.github.io
[vscode]: https://code.visualstudio.com
