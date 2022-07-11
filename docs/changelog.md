# Changelog

The changelog is not an exhaustive list of changes between versions. For that, check the git log.

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
[sml-nj-doc]: https://www.smlnj.org/doc/smlnj-lib/index.html
