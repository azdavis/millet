# Manual

This is a manual for how to use Millet.

See also the [blog post](https://azdavis.net/posts/millet/) for an overview.

## A note about editors

In general, it should be possible to use Millet with any editor that supports language servers, but VS Code is the most well-supported editor.

### VS Code

We provide an official [VS Code][vs-code] extension in the [marketplace][].

Some Millet features are provided by the VS Code extension, not the language server. These features are thus not available in other editors.

### VSCodium and similar

In the above, when we say "VS Code", we really mean "VS Code or any similar compatible editor", like [VSCodium][]. This means you can use the official Millet extension in VSCodium.

Because only Microsoft's VS Code is allowed to use the Microsoft extension marketplace, Millet is also available on [Open VSX][ovsx] for other VS Code-compatible editors.

### Other editors

When using other editors, you will likely need to:

1. Put the `millet-ls` binary on your `$PATH`. You can either [download a pre-built binary for your platform][releases], or clone/download the repository and [build from source][build-from-source].
2. Write your own "glue code" to get your editor to use Millet for SML, ML Basis, and CM files.
3. Arrange for your editor to pass Millet the appropriate [initialization options](#language-server-initialization).

## Known issues

- Some features like `abstype` are not supported.
- CM support is incomplete.
  - The preprocessor is ignored.
  - Tool options are not supported.
  - String paths (wrapped in quotes) are not supported.
- ML Basis support is incomplete.
  - Most annotations are ignored.
  - String paths (wrapped in quotes) are not supported.
- The standard basis is made available to all files, regardless of whether files ask for it.
- Every file is entirely re-analyzed upon a single file change. This can make the server slow.
- Fixity declarations are not imported across files by default. You can enable importing fixity declarations with [a setting](#languagefixity-across-files), but this may make Millet slower.

## Usage

After opening your editor onto a folder, Millet will look for a "group file" directly contained in that folder.

A group file is either:

- a [ML Basis][mlb] (MLB) file (`.mlb`), or
- a [SML/NJ Compilation Manager][cm] (CM) file (`.cm`).

These files list out SML source files and other group files to organize the project.

For more exotic projects, you may wish to create an optional [`millet.toml`](#millettoml).

Note that a group file, or a `millet.toml` file pointing to a group file, **must** be present **directly** in the directory that you open your editor onto. It can't be in subdirectories, because Millet will not look in subdirectories, unless you tell it to via `millet.toml`.

If a file is not transitively reachable from the root group file, it **will not** be analyzed.

### ML Basis

This group file syntax is often used with [MLton](http://mlton.org). Its documentation is [here](http://mlton.org/MLBasis).

Generally, the syntax of ML Basis files is more similar to SML's own syntax than SML/NJ CM. MLB also has fewer and different features than CM.

### SML/NJ Compilation Manager

This group file syntax is used with SML/NJ. Its documentation is [here](https://www.smlnj.org/doc/CM/new.pdf).

One notable feature of CM that Millet supports is special treatment for ML-Lex and ML-Yacc files.

- ML-Lex files end in `.l` or `.lex`.
- ML-Yacc files end in `.y` or `.grm`.

Millet understands that CM will generate corresponding SML files from a given one of these source files.

- For ML-Lex files, there will be a `.sml` file generated.
- For ML-Yacc files, there will be both a `.sml` and `.sig` file generated.

The SML filename will be the same as the input filename with an extra `.sml` or `.sig` extension at the end. This is in addition to the existing extension of the input filename.

- Example: `Foo.l` will produce `Foo.l.sml`.
- Example: `Bar.grm` will produce `Bar.grm.sml` and `Bar.grm.sig`.

The information is summarized in the table below:

| Kind    | Short ext | Long ext | Produces `.sml` | Produces `.sig` |
| ------- | --------- | -------- | --------------- | --------------- |
| ML-Lex  | `.l`      | `.lex`   | Yes             | No              |
| ML-Yacc | `.y`      | `.grm`   | Yes             | Yes             |

## Configuration

There are four places where Millet can be configured:

- [`millet.toml`](#millettoml). This is for project-wide settings.
- [VS Code settings](#vs-code-settings). This is for user-specific settings.
- [Language server initialization](#language-server-initialization). This is for advanced use-cases and/or non-VS-Code editors.
- [ML Basis annotations](#ml-basis-annotations). This is for specific files.

### `millet.toml`

Millet can be configured with a `millet.toml` in the workspace root. It is a [TOML][] file with the following format:

```toml
version = 1
[workspace]
root = "foo.cm"
[workspace.path-vars]
FOO = { value = "woof" }
BAR = { path = "bork" }
QUZ = { workspace-path = "pant" }
[diagnostics]
5011.severity = "warning"
4015.severity = "error"
5029.severity = "ignore"
[language]
fixity-across-files = true
[language.exp]
while = false
[language.dec]
functor = true
[language.val]
"=" = false
[language.structure]
"Ref" = false
[language.successor-ml]
or-pat = false
```

#### `version`

- Type: `number`
- Valid values:
  - `1`: the first version of the config file.

The version of the config file. At time of writing, it must be exactly `1`.

#### `workspace.root`

- Type: `string`

Sets the path(s) to the root group file(s). Glob syntax is available.

As mentioned, a group file is either:

- a [ML Basis][mlb] file (`.mlb`), or
- a [SML/NJ Compilation Manager][cm] file (`.cm`).

In the case where there is **exactly one** group file in the root project folder, Millet infers that group file to be the root group file. In this case, there is no need to use the `workspace.root` setting, since it is inferred.

But if not, Millet will not know how to analyze your project.

##### Disambiguation with multiple root group files

For instance, if you open your editor Millet onto `foo` in this example, Millet will not know how to analyze the project, because there is more than one group file directly inside `foo`. Namely, there is both `foo/sources.cm` and `foo/test.cm`:

```
foo
├── bar.sml
├── quz.sml
├── sources.cm
├── test.cm
└── tests.sml
```

To resolve this, create `foo/millet.toml` to specify which group file should be the root:

```toml
version = 1
workspace.root = "test.cm" # or sources.cm
```

##### Intentionally globbing multiple root group files

In this example, there is **no** group file directly in `foo`, so Millet will again not know how to proceed. Note Millet does not recur into the subdirectories to find group files.

```
foo
├── bar
│   ├── hello.sml
│   └── sources.cm
└── quz
    ├── goodbye.sml
    └── sources.cm
```

Because each subdirectory has a group file named `sources.cm`, we can use glob syntax in `foo/millet.toml`:

```toml
version = 1
workspace.root = "*/sources.cm"
```

This will pick up both `foo/bar/sources.cm` and `foo/quz/sources.cm`. It will also pick up any future subdirectories with a `sources.cm` put directly into `foo`.

#### `workspace.path-vars.<var>`

- Type: `{ value: string } | { path: string } | { workspace-path: string }`

How to expand the `<var>` inside paths in group files.

- If the value is a `value`, the value is used unchanged.
- If it is a `path`, then the value is expanded into a full path relative to the `millet.toml` file.
- If it is a `workspace-path`, then the value is expanded into a full path relative to the workspace root group file (of which there may be many, because `workspace.root` can be a glob).

Given this setup of files:

```
/users/foo
├── bar
│   ├── hello.sml
│   └── sources.cm
├── quz
│   ├── goodbye.sml
│   └── sources.cm
└── millet.toml
```

Where the `millet.toml` has these contents:

```toml
version = 1
[workspace]
root = "*/sources.cm"
[workspace.path-vars]
V1 = { value = "a" }
V2 = { path = "b" }
V3 = { workspace-path = "c" }
```

Then the given variable, when referenced inside the given file, has the given value:

| Var  | File                 | Value               |
| ---- | -------------------- | ------------------- |
| `V1` | `foo/bar/sources.cm` | `a`                 |
| `V1` | `foo/quz/sources.cm` | `a`                 |
| `V2` | `foo/bar/sources.cm` | `/users/foo/b `     |
| `V2` | `foo/quz/sources.cm` | `/users/foo/b `     |
| `V3` | `foo/bar/sources.cm` | `/users/foo/bar/c ` |
| `V3` | `foo/quz/sources.cm` | `/users/foo/quz/c ` |

Note that variables are referenced differently in different kinds of group files. Given that a variable named `A` is defined in `millet.toml`:

- In ML Basis files, it is referenced as `$(A)`.
- In SML/NJ CM files, it is referenced as `$A`.

Variable names should contain ASCII alphanumeric characters and `-`, and not begin with a numeric character.

In SML/NJ CM files **only**, a variable name can be empty or contain `-`.

Certain path variables are used for basis paths. These paths tell a SML implementation where the source files that implement the standard basis and other libraries are. The variables are:

- In MLB files:
  - `SML_LIB`
- In CM files:
  - The "empty variable" (written `$` when referenced in a CM file)
  - `SMLNJ-LIB`

However, Millet already comes with built-in definitions for most of the standard basis. Thus, if these special variables are referenced in a path, but not defined in `millet.toml`, Millet ignores the entire path instead of erroring.

If you do define these special variables in `millet.toml`, then Millet will attempt to process any paths that contain those variables normally. However, this may not always be desirable:

- If your group file references a basis path that Millet already has built-in definitions for, those built-in definitions could clash with the definitions at that basis path.
- Because the standard basis is special, the files that implement it may not be fully conformant SML. Millet may not be able to properly process these files.

You can use the [`milletIgnore`](#milletignore) ML Basis annotation for these situations. SML/NJ CM does not have a comparable "annotations" feature.

#### `diagnostics.<code>.severity`

- Type: `string`
- Valid values:
  - `"ignore"`: the diagnostic is not reported.
  - `"warning"`: the diagnostic is reported as a warning.
  - `"error"`: the diagnostic is reported as an error.

Overrides the default severity for this [diagnostic](#inline-diagnostics).

The `<code>` must be a positive integer.

#### `language.fixity-across-files`

- Type: `boolean`
- Default: `false`

Whether fixity declarations (`infix`, `infixr`, and `nonfix`) can take effect across files.

When this is `false`, each file is parsed starting with the default fixity environment provided by the standard basis. This means we can incrementally re-parse files and/or parse files in parallel since there are no inter-file dependencies when parsing. (At time of writing, we do not currently do this.)

When this is `true`, we cannot do the above things, and we must also use more memory to store the fixity environments used when parsing each file.

#### `language.dec.<kind>`

- Type: `boolean`
- Default: `true`

Whether the `<kind>` of declaration/specification is allowed.

Valid `<kind>`s:

- `val`
- `fun`
- `type`
- `datatype`
- `datatype-copy`
- `exception`
- `open`
- `fixity` (covers `infix`, `infixr`, and `nonfix`)
- `local`
- `structure`
- `signature`
- `functor`
- `exp` (expression declarations)
- `include` (technically a specification, not a declaration)

#### `language.exp.<kind>`

- Type: `boolean`
- Default: `true`

Whether the `<kind>` of expression is allowed.

Valid `<kind>`s:

- `int-lit`
- `real-lit`
- `word-lit`
- `char-lit`
- `string-lit`
- `path`
- `record`
- `selector`
- `paren`
- `tuple`
- `list`
- `seq`
- `let`
- `app`
- `infix`
- `typed`
- `andalso`
- `orelse`
- `handle`
- `raise`
- `if`
- `while`
- `case`
- `fn`

#### `language.val.<path>`

- Type: `boolean`
- Default: `true`

Whether the `<path>` is allowed.

The path must be a valid, fully qualified path in the standard basis library, like `Real.==` or `+`.

Because paths can have special characters in them, namely `.`, you may need to use TOML's quoted path syntax. Like this:

```toml
[language.val]
"List.tabulate" = false
```

Note that some standard basis library declarations are re-declared at different paths. To disallow them entirely, you must (currently) specify all possible paths. For instance, you should specify both `hd` and `List.hd` to disallow usage of the list head function.

No error is currently emitted when disallowing a path that does not exist. To be sure you spelled the path correctly, try running Millet with the given config file, and see if errors are correctly emitted when you try to use the value.

#### `language.structure.<path>`

- Type: `boolean`
- Default: `true`

Whether the `<path>` is allowed.

See docs for [`language.val.<path>`](#languagevalpath).

#### `language.successor-ml`

Configuration for [Successor ML][succ-ml] features.

#### `language.successor-ml.do-dec`

- Type: `boolean`
- Default: `false`

Whether `do` declarations are allowed.

```sml
(* do dec *)
do e

(* equivalent to *)
val () = e
```

#### `language.successor-ml.opt-bar`

- Type: `boolean`
- Default: `false`

Whether `|` are allowed before the first `datatype`, `fn`, `case`, `handle`, or `fun` case.

```sml
datatype thing =
| Foo
| Bar of int

fun check x =
  case x of
  | 1 => 2
  | 3 => 4
  | _ => 5
```

#### `language.successor-ml.opt-semi`

- Type: `boolean`
- Default: `false`

Whether a trailing `;` is allowed in the expression sequence of a `let` expression.

```sml
(* trailing ; *)
val () =
  let
    val x = 3
    val y = 4
  in
    foo x;
    bar y;
  end

(* equivalent to *)
val () =
  let
    val x = 3
    val y = 4
  in
    foo x;
    bar y;
    ()
  end
```

#### `language.successor-ml.or-pat`

- Type: `boolean`
- Default: `true`

Whether or-patterns are allowed.

```sml
datatype thing = Foo of int | Bar of int
fun extract (Foo x | Bar x) = x
```

#### `language.successor-ml.exp-row-pun`

- Type: `boolean`
- Default: `false`

Whether expression row punning is allowed.

```sml
(* row punning *)
fun incB {a, b, c} = {a, b = b + 1, c}

(* equivalent to *)
fun incB {a, b, c} = {a = a, b = b + 1, c = c}
```

#### `language.successor-ml.vector`

- Type: `boolean`
- Default: `false`

Whether vector expressions and patterns are allowed.

```sml
val vec : int vector = #[1, 2, 3]

fun foo (xs : int vector) : int =
  case xs of
    #[] => 1
  | #[x, 4] => x
  | #[a, b, _] => a * b
  | #[_] => 5
  | _ => 6
```

### VS Code settings

Millet has VS Code specific settings, which are stored as [JSON][]. You may need to reload VS Code and/or Millet to pick up the changes.

<!-- @begin vscode-config -->

#### `millet.format.engine`

- Type: `string`
- Default: `"none"`
- Valid values:
  - `"none"`: No formatting.
  - `"naive"`: Naive formatting.
  - `"smlfmt"`: Formatting provided by [`smlfmt`](https://github.com/shwestrick/smlfmt), which must be installed in your `$PATH`.

**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

How to [format](#formatter) open SML files on save.

#### `millet.server.diagnostics.ignore`

- Type: `string`
- Default: `"after-syntax"`
- Valid values:
  - `"none"`: Ignore no diagnostics, i.e. send all diagnostics.
  - `"after-syntax"`: If there are syntax diagnostics (lex error, parse error, etc), send only those, and ignore e.g. statics diagnostics.
  - `"all"`: Ignore all diagnostics.

What [diagnostics](#inline-diagnostics) to ignore.

#### `millet.server.diagnostics.moreInfoHint.enable`

- Type: `boolean`
- Default: `true`

Show a hint on diagnostic messages about clicking the error code number for more information.

#### `millet.server.diagnostics.onChange.enable`

- Type: `boolean`
- Default: `false`

Send diagnostics when file contents change before saving.

#### `millet.server.enable`

- Type: `boolean`
- Default: `true`

Enable the language server.

#### `millet.server.fileSystemWatcher.enable`

- Type: `boolean`
- Default: `true`

Use a file system watcher to send events when files change, if one is available.

#### `millet.server.hover.token.enable`

- Type: `boolean`
- Default: `true`

Show information about tokens on hover.

#### `millet.server.path`

- Type: `string`
- Default: `""`

Path to the `millet-ls` executable.

When set to the empty string `""` (the default), use the path to the one that's pre-built and bundled with the extension.

<!-- @end vscode-config -->

### Language server initialization

If you're using VS Code, the VS Code extension automatically passes the appropriate required custom initialization options to the language server process when starting it up.

If you're not using VS Code, you will have to arrange to pass these initialization options through some other editor-specific means.

The initialization options are a subset of the VS Code config, but rearranged and renamed slightly. Consult the implementation of the VS Code extension to see what options are sent. Additionally, consult the documentation for the VS Code configuration to see what types the configuration options must be.

### ML Basis annotations

Millet knows about some [ML Basis annotations][mlb-ann]. The ones not mentioned here are ignored.

#### `milletDiagnosticsIgnore`

How to ignore [diagnostics](#inline-diagnostics) for the files in the annotated basis declaration.

Possible arguments:

- `true`: Ignore all diagnostics.
- `all` (discouraged): Ignore all diagnostics.
- `false`: Report diagnostics as normal.

For example, suppose we have 3 files, each quite similar, and each containing a type error:

| Filename | Contents       |
| -------- | -------------- |
| `a.sml`  | `val () = "a"` |
| `b.sml`  | `val () = "b"` |
| `c.sml`  | `val () = "c"` |

Even though each file would normally emit a type error, given the following root ML Basis file, the errors are reported in `a.sml` and `c.sml` only, and not `b.sml`:

```text
a.sml
ann "milletDiagnosticsIgnore true" in
  b.sml
end
c.sml
```

Using `false` as the argument to `milletDiagnosticsIgnore` can override a previous `true`.

Given this arrangement, errors would be reported in `b.sml` and `c.sml`:

```text
ann "milletDiagnosticsIgnore true" in
  a.sml
  ann "milletDiagnosticsIgnore false" in
    b.sml
  end
end
c.sml
```

### `milletIgnore`

Whether to ignore all content in the basis declaration.

Possible arguments:

- `true`: Ignore the whole basis declaration contained inside.

The text of the contained basis declaration must parse as a basis declaration. But it, and any files mentioned inside it, is otherwise totally ignored, and not processed at all. That means no definitions from those files will be available.

For instance, given these SML files:

| Filename | Contents        |
| -------- | --------------- |
| `a.sml`  | `val a = 3`     |
| `b.sml`  | `val a = true`  |
| `c.sml`  | `val _ = a + 1` |

And this MLB file:

```text
a.sml
ann "milletIgnore true" in
  b.sml
end
c.sml
```

There will be no errors, since `b.sml`, which would have shadowed `a` to be a `bool` and caused a type error in `c.sml`, is completely ignored.

Unlike `milletDiagnosticsIgnore` you cannot enable this annotation and then re-disable it inside itself.

## Features

Millet has a bevy of features to help you read, write, and understand SML code.

### (VS Code only) Syntax highlighting

VS Code will highlight keywords, literals, comments, etc in these files:

| Full name                  | Short name | Extensions             |
| -------------------------- | ---------- | ---------------------- |
| Standard ML                | SML        | `.sml`, `.sig`, `.fun` |
| ML Basis                   | MLB        | `.mlb`                 |
| SML/NJ Compilation Manager | CM         | `.cm`                  |

### (VS Code only) Bracket and comment configuration

VS Code knows about things like comment delimiters and what kinds of brackets should be auto-matched in these files.

This allows for features like:

- Use the "toggle comment" keybinding in these files to comment out a line.
- Type e.g. a `{`, and the editor will auto-insert the matching `}`.

### (VS Code only) Snippets

In VS Code, all of the above files have some pre-defined snippets. These can be triggered by typing the "prefix" word and then hitting a "commit character" (like tab).

#### ML Basis

<!-- @begin mlb-snippets -->

- `let` exp
- `bas` exp
- `local` dec
- `ann` dec

<!-- @end mlb-snippets -->

#### SML/NJ CM

<!-- @begin sml-nj-cm-snippets -->

- `group` desc
- `library` desc

<!-- @end sml-nj-cm-snippets -->

#### SML

<!-- @begin sml-snippets -->

- `let` exp
- `case` exp
- `if` exp
- `handle` exp tail
- `local` dec
- `datatype` dec
- `fun` dec
- `structure` dec
- `signature` dec
- `functor` dec
- `print` with newline exp

<!-- @end sml-snippets -->

### Inline diagnostics

Millet will analyze source (SML) and group (MLB/CM) files and report diagnostics directly on the offending area of the file.

Each diagnostics has a default severity, e.g. "error" or "warning". This can be overridden with [`diagnostics.<code>.severity`](#diagnosticscodeseverity) in `millet.toml`. A severity of `"ignore"` ignores (i.e. disables) the diagnostic.

Diagnostics can be ignored for an entire file or set of files with the [`milletDiagnosticsIgnore`](#milletdiagnosticsignore) ML Basis annotation. Use:

```
ann "milletDiagnosticsIgnore true" in
  foo.sml
  bar.sml
end
```

Diagnostics can be ignored for all files with the [`millet.server.diagnostics.ignore`](#milletserverdiagnosticsignore) VS Code setting.

### Hover for info

In SML files, hover over something to get more information on it.

Millet shows things like:

- The type of expressions or patterns.
- Documentation for an item.
- Documentation for tokens.

See the section on [doc comments](#doc-comments) to provide your own documentation for items.

### Inlay hints

In SML files, Millet can show inlay hints with type annotations.

In VS Code, inlay hints can be enabled or disabled across the entire editor via `editor.inlayHints.enabled`.

### Jump/peek definition

In SML files, Millet allows jumping to or peeking the definition of named items, like variables.

### Completions

Millet provides completions for the current cursor location. Completions can be triggered by typing a regular name or `.` after a name. When typing `.`, Millet will traverse the existing path.

In this example, Millet provides the given completions at the given cursor location.

```sml
structure Foo = struct
  val bar = 3
  val quz = "hi"
end

val _ = Foo.
(**         ^ completions: bar, quz *)
```

### Code action: fill case

When your cursor is over the `case` or `of` keywords of a `case` expression, Millet can fill in the case with arms for each variant of the type of the head expression.

### Document symbols

Millet can show all the symbols in a document, and information about those symbols.

### Find all references

Millet supports finding references to a symbol.

### Doc comments

Millet allows defining documentation comments on items to be shown on hover.

Use this comment style:

```sml
(*!
 * `inc x` returns one more than `x`.
 *)
fun inc x = x + 1
```

So, put `(*!` on its own line, then the doc comment in Markdown with leading `*` on each line, and then `*)` on its own line.

### Holes

Millet allows writing `...` or `_` as a "hole" in various contexts (expression, type, declaration, etc) in SML files. They are parsed, but rejected in later stages of analysis.

This allows writing "example" code that actually parses. In the case of expression holes, the error message also reports the inferred type of the hole.

### Formatter

**WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.**

Millet can automatically format your open SML files. Set `millet.format.engine` to something other than `"none"` in your settings, then reload your editor. Now, when saving an open file, Millet will format.

- If you use `"naive"`, the formatter is built-in to Millet.
- If you use `"smlfmt"`, you'll need to install [`smlfmt`][smlfmt] in your `PATH`.

There are some other caveats.

#### Experimental

The first and most obvious caveat is the one above, in big bold uppercase.

Most Millet features only **read** your files, and produce output only in the form of things like

- diagnostics
- hover information
- locations for definition sites

If Millet has a bug, then the worst that happens is it provides incorrect information about your files. This can be safely ignored with no damage to your files.

The formatter, however, **rewrites** your files. If there is a bug in the formatter, in the worst case, it could overwrite your files with

- invalid syntax
- garbage
- nothing at all (i.e. essentially delete your files)

So, the formatter is disabled by default, and great care should be taken when using it.

#### Long lines

The naive Millet formatter employs exceedingly unsophisticated strategies to break code across many lines. What this means is that large expressions (e.g. a function call expression with many long arguments) may be formatted all on one line.

The suggested workaround is to use a `let ... in ... end` expression and split out sub-expressions into variables. So instead of:

```sml
Boop.beep (if bar x then quz y else Fee.Fi.Fo.fum z) (fn res => s (blab :: res)) (fn () => k []) (fn (x, ac) => ac andalso x) (xs @ ys @ zs)
```

Try something like:

```sml
let
  val fst = if bar x then quz y else Fee.Fi.Fo.fum z
  fun succeed res = s (blab :: res)
  fun fail () = k []
  fun andBoth (x, ac) = ac andalso x
  val all = xs @ ys @ zs
in
  Boop.beep fst succeed fail andBoth all
end
```

An arguably good thing about this is that it might improve readability anyway.

#### Comments

The naive formatter completely gives up on formatting the file if a comment appears in a place that the formatter doesn't know how to deal with.

The **only** kind of comment the formatter even **attempts** to deal with are comments directly above declarations, like these:

```sml
(* a is the first letter *)
val a = 1

(* id x returns x unchanged *)
fun id x = x

(* isomorphic to bool *)
datatype dayKind = Weekday | Weekend
```

Comments in other positions, like inside expressions, are not supported:

```sml
val uh =
  if foo then
    (* always bar when foo *)
    bar
  else
    (* fall back to quz otherwise *)
    quz
```

When the formatter cannot format a file, it simply does nothing, and the file will not be formatted. It emits warnings pointing at the comments that prevented formatting.

#### Configuration

There are no options to configure the formatter.

This is by design. More options means more [bikeshedding][bike-shed].

The formatter does, however, respect the editor-configured tab size.

[cm]: https://www.smlnj.org/doc/CM/new.pdf
[marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.millet
[mlb]: http://mlton.org/MLBasis
[ovsx]: https://open-vsx.org/extension/azdavis/millet
[vs-code]: https://code.visualstudio.com
[vscodium]: https://vscodium.com
[toml]: https://toml.io/en/
[json]: https://www.json.org/json-en.html
[smlfmt]: https://github.com/shwestrick/smlfmt
[mlb-ann]: http://mlton.org/MLBasisAnnotations
[bike-shed]: https://en.wikipedia.org/wiki/Law_of_triviality
[succ-ml]: http://mlton.org/SuccessorML
[releases]: https://github.com/azdavis/millet/releases
[build-from-source]: /README.md#development
