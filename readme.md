# Millet

A [language server][lang-server] for [Standard ML][sml-def], with a
corresponding [Visual Studio Code][vscode] language client extension.

This project is alpha-quality software. There are
[many important things](doc/todo.md) not yet implemented.

## Build

You'll need:

- `cargo` and `rustc`
- `npm` and `node`
- `git`

```
$ git clone https://github.com/azdavis/millet.git
$ cd millet
$ cargo build
$ cd extensions/vscode
$ npm install
$ npm run build
```

## Naming

"Millet" has M and L in it, in that order. So does "Standard ML".

Also, birds eat millet, and a bird named Polly Morphism is the mascot for
[15-150][cmu150], Carnegie Mellon's introductory functional programming course
taught in Standard ML.

[sml-def]: https://smlfamily.github.io/sml97-defn.pdf
[lang-server]: https://microsoft.github.io/language-server-protocol/
[vscode]: https://code.visualstudio.com
[cmu150]: http://www.cs.cmu.edu/~15150/
