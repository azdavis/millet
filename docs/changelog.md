# Changelog

The changelog is not an exhaustive list of changes between versions. For that, check the git log.

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
- Support `...` pattern rows (and thus `#` selectors).
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
