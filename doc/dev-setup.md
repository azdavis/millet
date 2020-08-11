# Development

First, clone the repository with `git` and cd inside.

For development of the main codebase in `crates`, install Rust, probably with
[rustup][]. Then run `cargo build`.

For development of the VSCode extension in `extensions/vscode`, first install
[node][]. Then cd into `extensions/vscode`, and run `npm install`.

If you're using VSCode, open the root directory of this repository in VSCode to
get extension recommendations for a pleasant Rust developer experience.

VSCode also lets you try out the language client extension from the Run panel.
Open the Run panel (play button with bug), select 'extension' in the drop down,
and press the green play button. This will build the language server and client
(it might take a bit), and then open a new VSCode window with the extension
enabled. From there you can open up a SML file and try it out.

The scripts in `bin` require a POSIX `sh`.

[rustup]: https://rustup.rs
[node]: https://nodejs.org/en/
