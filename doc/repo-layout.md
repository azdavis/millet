# Repository layout

- `.vscode` contains configuration for Visual Studio Code, which is invaluable
  when developing the VSCode language client extension.
- `bin` contains various scripts.
  - `bin/ck-sml-defn` checks whether all of the rules of the statics have been
    mentioned in the implementation of the statics.
  - `bin/mk-test` makes `ok.sml` tests.
  - `bin/mk-vscode-ext` builds the VSCode language client extension and language
    server, and puts the language server binary near the built client.
  - `bin/run-test` runs tests in `tests`.
- `crates` is the primary location of code implementing Standard ML.
  - `crates/cli` contains a command-line interface which runs the lexer, parser,
    and typechecker from `crates/core` on a sequence of files.
  - `crates/core` contains the Standard ML lexer, parser, and typechecker.
  - `crates/ls` contains a language server which runs the lexer, parser, and
    typechecker from `crates/core` on files sent to it by the language client.
- `doc` contains documentation.
- `extensions` contains language client extensions for text editors to
  communicate with the language server.
- `tests` contains tests.
