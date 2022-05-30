# Millet

A [language server][lang-server] for [Standard ML][sml-def].

Also a corresponding [Visual Studio Code][vscode] extension.

## Warning

This project is alpha-quality software.

- There are [many important things](todo.md) not yet implemented.
- There are things that are shoddily implemented. Note the ignored tests.

## Development

`git clone` the repo, `cd` inside, and run `cargo xtask ci`.

If you're using VSCode, you can try out the VSCode extension:

1. Open the root directory of this repository in VSCode.
2. Open the Run panel from the activity bar (the play button with bug).
3. Select 'extension' in the drop down.
4. Press the green play button.

## Repository layout

- Most of the code is in `crates/`.
- Editor extensions are in `extensions/`.
- The "build system" is in `xtask/`. It requires rust, node, and git.

## Testing

Most tests are in `crates/tests/` and use `check()` like this:

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
[lang-server]: https://microsoft.github.io/language-server-protocol/
[node]: https://nodejs.org/en/
[rustup]: https://rustup.rs
[sml-def]: https://smlfamily.github.io/sml97-defn.pdf
[vscode]: https://code.visualstudio.com
