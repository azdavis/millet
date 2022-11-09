# Changelog

The changelog is not an exhaustive list of changes between versions. For that, check the git log.

Millet technically follows [SemVer][sem-ver], but the major version is zero, and it probably will be [for a while][zero-ver]. This means Millet makes **absolutely no** stability guarantees between versions.

The versioning system is basically the following:

- The version is split into three numbers, `major.minor.patch`.
- Usually, we increment the "patch" version.
- We increment the "minor" version either:
  - After many "patch" versions.
  - If there's a really "big" change.
- As mentioned, the "major" version is 0.

## v0.5.14

- Improve performance slightly.
- Respect tab size options when formatting.
- Improve showing directories in some errors.
- Ignore CM preprocessor on the first line.
- Give more context on why types were mismatched.

## v0.5.13

- Show a hint by default about clicking the diagnostic code number for more information.
- Improve auto-formatting to not discard new edits.

## v0.5.12

- Improve range for statics errors on `case`, `let`, `local`, `struct`, and `sig`.
- Improve formatting for `fn`, `val`, `let`, parenthesized expressions, `withtype`, and functor declaration arguments.
- Warn when formatting is enabled but a comment prevents formatting.
- Correctly error when using e.g. `if` or `case` as an argument to an infix operator without parentheses.
- Add an error for `fun` with no parameters.

## v0.5.11

- Add a specific diagnostic for unmatched closing delimiters.
- Do not report statics errors if there are syntax errors by default.
- Warn about shadowing the name of a containing `fun` in matcher patterns. (This is often a sign that a `case` or similar needs parentheses around it.)

## v0.5.10

- Report errors with relative paths where possible.
- Ignore some "realize type" errors when using `where S = T`. (This is probably unsound, but it silences errors seen in NJ-flavored SML accepted by SMLNJ.)

## v0.5.9

- Improve performance by using smaller integer types.
- Improve performance by using cheap integer comparisons instead of iterating over the entire basis.

## v0.5.8

- Warn when using `case` on a `bool`.
- Allow using the highly experimental SML formatter. It defaults to off.

## v0.5.7

- Warn on mismatched sugar content for functor application.
- Warn when using `@` for empty lists (as either argument) or singleton lists (as the first argument).

## v0.5.6

- Warn when using `=` on some special constants.
- Publish to Open VSX as well as the VS Code marketplace.

## v0.5.5

- Check for type variable scope escape.
- Check for invalid `\^` string escapes.
- Fix pattern matching analysis for strings and characters.
- Improve checking of `where type` and `sharing type`.
- Report type names as fully qualified.

## v0.5.4

- Fix some panics when encountering strange programs.

## v0.5.3

- Support `source` exports in SML/NJ CM export lists.

## v0.5.2

- Support union, intersection, and difference in SML/NJ CM file export lists.
- Improve SML/NJ syntax highlighting.

## v0.5.1

- Add more suggestions for booleans and options.
- Add MLton libraries.

## v0.5.0

- Correctly check for equality of type functions. This means we now correctly reject the following example program:
  ```sml
  structure S :
    sig    type ('a, 'b) t = 'a * 'b end =
    struct type ('a, 'b) t = 'b * 'a end
  ```
- Change error severity to kebab case (e.g. `warning` instead of `Warning`).
- Allow ignoring errors with a severity of `ignore`.
- Automatically determine the order in which to analyze files for CM.
- Handle `group(path)` in CM.

## v0.4.2

- Improve `where S = T` involving polymorphic types.
- Allow shadowing constructors with `val rec`.
- Treat type variables bound at `val` or `fun` as out of scope in the right-hand side of `datatype` and `type` declarations.
- Remove a cap on the number of errors the server may report.

## v0.4.1

- Emit an error for numeric literals with trailing alphabetic characters.
- Parse specifications as declarations and reject invalid ones later.
- Emit more specific errors for some parse failures.

## v0.4.0

- Emit a better error for type variable sequences on `val` specifications.
- Warn on some cases of unnecessary parentheses.
- Warn on unnecessary usage of `op`.
- Warn on some overly complex expressions involving `bool`s.
- Warn on `case` expressions with only one arm.
- Warn on unnecessary usage of `;`.
- Warn on multiple types on one pattern.

## v0.3.14

- Add a `println` snippet.
- Add SML of NJ libraries.

## v0.3.13

- Emit warning on some unused variables.
- Allow overriding severity for errors.

## v0.3.12

- Allow top-level expressions.
- Emit a better error when an expression is in an invalid position, like in `let ... in`.
- Improve handling for language clients that do not support dynamic capability registration, by publishing diagnostics when open files are changed/saved.
- Improve error for invalid fixity declarations.

## v0.3.11

- Add an error code for no workspace root.
- Support `group(-)` CM exports.
- Add `Real64` and `PackReal64{Big, Little}`.
- Allow real literal overloads for e.g. `LargeReal.real`.
- Reword and improve some token docs.

## v0.3.10

- Add more suggestions, not just restricted to keywords, e.g. suggest `Int` for `Integer`.
- Fix panics when checking invalid patterns that appear to be constructors but are not.

## v0.3.9

- Emit better errors for unsupported CM exports like `source(-)` and `group(-)`.
- Syntax highlight the `source` keyword in CM files.
- Emit a better error for structure-level declarations in invalid positions.
- Suggest possible keywords when encountering an undefined identifier whose name is similar to a keyword.
- Improve token docs to add more "contrast with" (e.g. contrast `datatype` with `type`).
- Only dynamically register capabilities if the client supports it.
- Show token docs even when the token is part of a parse error.
- Emit a better error for `op andalso` and `op orelse`.

## v0.3.8

- Add a button to errors displayed as a notification, to view more information about the error.

## v0.3.7

- Add option for showing information about tokens on hover (default true).
- Add option for re-calculating diagnostics when files change without saving (default false).
- Add docs for std basis top-level aliases (e.g. `map` is an alias for `List.map`).
- Parse (but reject later) `<pat1> as <pat2>`.

## v0.3.6

- Change license from MIT to MIT OR Apache-2.0.

## v0.3.5

- Change the setting for the language server path to have type string, default `""`. This means it's editable from the VS Code UI more easily.
- Rename the setting to enable the language server to 'enable'.

## v0.3.4

- Change doc comment syntax.

## v0.3.3

- Add doc for primitive types, like `int` and `bool`.
- Improve doc for `->`.
- Fix syntax highlighting for real literals.
- Rename and doc VS Code config.
- Add VS Code config to set the server path.
- Mention the [community Discord][discord], for discussion and support.

## v0.3.2

- Allow for doc comments written `(*! ... *!)`. The delimiters must be on their own line, and the content is parsed as Markdown.

## v0.3.1

- Add docs for std lib extras.
- Ignore `$(SML_LIB)` paths in MLB.
- Ignore `$` and `$SMLNJ-LIB` paths in CM.
- Allow using `{ path = ... }` and `{ value = ... }` for `workspace.path-vars` in the config.
- Improve logo quality.

## v0.3.0

- Stop parsing sooner if an error is encountered in some cases, leading to less error spam.
- Report unknown types as `_`.
- Tweak error codes.
- Parse `...` as a declaration, expression, and type hole.
- Add docs for keywords (including punctuation) on hover.
- Fix a panic with `let` expressions with nothing between `in` and `end`.
- Improve generalization of type variables by adapting the approach in "Efficient ML Type Inference Using Ranked Type Variables" (doi:10.1145/1292535.1292538).
- Add icon, by [Yixin He][yixin].

## v0.2.11

- Emit an error for the value restriction.
- Add more error codes for errors.
- Display errors more consistently in the CLI.
- Fix crashes when interacting with unsaved additions at the end of the file.
- Reposition error ranges for `case` expressions.
- Add "fill case" quick fix for `case` expressions.

## v0.2.10

- Reject empty `open` and `include`, and `fun`s with no arguments.
- Process paths with `$(VARIABLES)` in them. (Note that default variables like `$(SML_LIB)` are not set.)
- Add more std basis extra, like `Ref` and `Either`.
- Add syntax highlighting for upper-case identifiers (conventionally the names of structures, signatures, and functors).
- Add syntax highlighting for common types (like `int`).

## v0.2.9

- Get ARM builds working again.
- Parse, but reject in later stages, some Successor ML features, like:
  - `do` declarations.
  - Leading `|` before cases for `fun`, `fn`, `case`, and `handle`.
  - `withtype` in specifications.
  - Expression row punning for records (like `val _ = {a, b}`).

## v0.2.8

- Greatly improve analysis of ML Basis files.
- Ignore all paths containing `$`, not just starting with it.
- Add `Word16` as another word structure and overloaded type.
- Add error codes and descriptions for more errors.

## v0.2.7

- Improve docs for errors.
- (internal) Simplify handling of the "root" sequence of top declarations.
- (internal) Update to a language-util that makes rustfmt no longer a hard dependency to build Millet.
- (internal) Move SML library files to their own crate.
- Stop making distributions for ARM. (They weren't working anyway; they were just x64 builds that we were calling ARM builds.)

## v0.2.6

- Add type and expression holes (`_`) as valid syntax, but reject them in later stages.
- Tweak error codes.

## v0.2.5

- Add syntax highlighting for uppercase/lowercase CM keywords.
- Mention the config file name in the "multiple root groups" error.
- Update the vscode readme to link to the [blog post][blog] and mention ML Basis.

## v0.2.4

- Fix definition of `structure IO` in the std basis libraries.
- Improve handling of type aliases in signatures.
- Improve handling of `datatype` to allow the value constructors to reference all other datatypes being defined (as with `and`), as well as any `withtype` types.
- Improve handling of `where type` when the path does not use all of the type variables, as in `where type 'a t = int`.
- Improve handling of `where type` with type variables to not incorrectly swap the order of the variables in some cases.
- Add more optional standard basis library definitions, and set some equal to each other, like `Word32` = `Word`. This is technically an implementation detail, but it is depended on in e.g. [cmlib][].
- Further improve reporting of overload errors.
- Add overloads for `int`, like `IntInf`.

## v0.2.3

- Allow upper and lower case `Group`, `Library`, etc in CM files.
- Add more std basis structures, like `Word32` and `PackWord32Big`.
- Add initial ML Basis support. It is very rudimentary, and basically only works for ML Basis files that are just lists of files.
- Improve syntax highlighting for ML Basis files.
- Improve reporting of overloads in errors.
- Allow word literals to have overloaded type.

## v0.2.2

- Improve types for `TextIO` with a hack, to paper over the invalid SML used in the std basis docs.
- Add optional functors from the std basis libs.
- Improve error locations and messages for CM errors.
- Do not panic if a type in a signature has the wrong number of type arguments.
- Add SML/NJ libraries from the [website][sml-nj-doc].

## v0.2.1

- Mark `it` as a special name in the syntax highlighting.
- Allow more whitespace, like carriage return.
- Improve error for unknown file class in CM files.
- Allow any single CM file to be the root CM file, not just `sources.cm`.
- Allow a config file for setting the single root CM file if more than one CM file exists in the root.

## v0.2.0

- First [publicly announced][blog] release.
- Show some CM, etc errors on the files themselves instead of in notifications if possible.
- Add 15-150 specific docs, update other docs.
- Improve logic for ignoring `$` paths in CM.

## v0.1.14

- Add initial standard basis library documentation.
- Improve matching on `datatype`s from signatures that were opaquely ascribed.
- Improve matching on records with `...` rest pattern rows.
- Improve display of record patterns in error messages.

## v0.1.13

- Add initial goto type def support.
- Greatly improve performance of statics with faster cloning.
- Add initial support of std basis documentation shown on hover (`Option`, `List`).

## v0.1.12

- Add initial goto def support for values, types, structures, signatures, and functors.
- Add initial hover support, showing the type of expressions, patterns, and types.
- Display "meta" type variables better: `?a`, `?b`, etc instead of always `_`.

## v0.1.11

- Add initial ML Basis support.
- Support `...` pattern rows, and thus `#` selectors.
- Improve overload support.
- Support some deviations from the Definition:
  - `signature` and `functor` in `local`.
  - `where S = T` NJ-style derived form.
  - Or patterns.

## v0.1.10

- Add some non-standard std basis items like `Fn` and `Random`.
- Allow `(op *)`.
- Handle the `sharing` derived form.
- Improve support for pattern rows.

## v0.1.9

- Add an error reference.
- Ignore (but do not parse error) `$` paths in CM.

## v0.1.8

- Add most of the std basis.
- Support CM files referencing other CM files.

## v0.1.7

- Add snippets.
- Fix issues with phantom types.
- Improve overload support.
- Parse more derived forms, like `type a = b` in specs.
- Add initial std basis support (`List`, `Option`, `ListPair`).

## v0.1.6

- Use CM files to discover what files to analyze.

## v0.1.5

- Add initial CM support (syntax highlighting, etc).
- Show witnesses to non-exhaustive matches.

## v0.1.4

- Initial support for most of Modules.

## v0.1.3

- Add more errors when lowering.
- Add architecture doc.
- Add initial Modules support.

## v0.1.2

- Initial working release for windows.
- Use different esbuild settings for debug and release builds.

## v0.1.1

Initial working release, now using `esbuild`.

## v0.1.0

Initial release, with some support for Core, none for Modules.

Although this is the first release with working CI to build and release the extension, the extension itself doesn't work at all because the `node_modules` were not packaged with the extension.

[blog]: https://azdavis.net/posts/millet/
[cmlib]: https://github.com/standardml/cmlib
[discord]: https://discord.gg/hgPSUby2Ny
[sem-ver]: https://semver.org
[sml-nj-doc]: https://www.smlnj.org/doc/smlnj-lib/index.html
[yixin]: https://yixinhe.me
[zero-ver]: https://0ver.org
