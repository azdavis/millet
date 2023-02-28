# Diagnostics

This is documentation for all the diagnostics Millet can emit, and what they mean.

If you're here, you may have followed a diagnostic code link from your editor. To turn off the hint about clicking those links in VS Code, set `millet.server.diagnostics.moreInfoHint.enable` to false.

## 1001

Millet failed to perform file or directory I/O with the filesystem. It could be that the path in question:

- does not exist.
- has insufficient permissions.
- is a directory when a file was expected, or vice versa.

To fix, inspect the error for the underlying cause.

## 1002

**NOTE:** This error is not currently emitted. It may be in the future.

A path wasn't contained in the root directory.

Millet requires all the files to be analyzed be ultimately contained within a single, "root" directory. Millet thus refuses to analyze files outside this root.

To fix, move the file into the root, or do not reference it.

## 1003

There were multiple "group files" in the root directory for the workspace.

Group files are SML/NJ Compilation Manager (`.cm`) or ML Basis (`.mlb`) files.

Given a workspace directory, Millet will look for group files directly in that directory. (By "directly", we mean not in sub-directories.)

If exactly one such group file is found, it is treated as the "root" group file. But if more than one was found, Millet emits this error, and requires that you choose which group file should be the root.

To fix, select which group you want to be the root group file with a `millet.toml` [config file][config], like this:

```toml
version = 1
workspace.root = "foo.cm"
```

## 1004

There were no "group files" in the root directory for the workspace.

Group files are SML/NJ Compilation Manager (`.cm`) or ML Basis (`.mlb`) files.

Given a workspace directory, Millet will look for group files directly in that directory. (By "directly", we mean not in sub-directories.)

If exactly one such group file is found, it is treated as the "root" group file. But if zero were found, Millet emits this error.

To fix, try any of the following:

- Create a group file in the top-level directory. The simplest group file is a ML Basis file listing all of the SML files in your workspace. For instance, you could create a file `sources.mlb` in your workspace root like this:

  ```mlb
  foo.sml
  lib/bar.sml
  many/directories/deep/quz.sml
  ```

  That is, each SML file is listed in order, one per line.

- Change the top-level directory, by opening your editor onto a different directory.
- Create a `millet.toml` file in the top-level directory pointing at a root group file.

  For instance, if you have opened your editor to `~/foo`, and you have a group file at `~/foo/bar/quz.mlb`, you can create `~/foo/millet.toml` with the contents:

  ```toml
  version = 1
  workspace.root = "bar/quz.mlb"
  ```

## 1005

A group file manually specified was not either a `.mlb` or `.cm` file.

To fix, only specify those such paths as group paths.

## 1006

Millet could not parse the config file as valid TOML.

To fix, ensure the TOML syntax is valid, and that the config file is of the [expected format][config].

## 1007

The config file had an invalid version.

To fix, only use version 1.

## 1008

There was an error when parsing a SML/NJ CM file.

To fix, use only the subset of CM syntax Millet understands. Some features, like the "preprocessor", tool options, and string paths, are not supported.

## 1009

There was an error when parsing a ML Basis file.

To fix, use only the subset of MLB syntax Millet understands. Some features, like string paths and annotations, are not supported.

## 1010

There was a cycle between files.

For instance, a cycle occurs if a group file attempts to include itself. As another example, a cycle also occurs if:

- A group file `X` attempts to include a group file `Y`, and
- that file `Y` also attempts to include `X`.

To fix, break the cycle.

## 1011

There was a duplicate name in a ML Basis file. For instance, `structure A and structure A` will trigger this error.

To fix, use different names, or remove the `and`. See also 5002.

## 1012

In a `millet.toml` config file, a key of the `diagnostics` table was not a valid error code.

A valid error code will be a positive integer.

To fix, make the key of the `diagnostics` table an error code.

## 1013

In a SML/NJ CM file, there was a `source(path)` export whose path was not in the list of files.

```sml-nj-cm
Library
  source(foo.sml)
is
  bar.sml
```

To fix, include the path in the list of files.

```sml-nj-cm
Library
  source(foo.sml)
is
  foo.sml
  bar.sml
```

## 1014

There was a glob pattern parse error in `workspace.root`.

Currently, Millet uses the [glob crate](https://docs.rs/glob/latest/glob) for glob parsing. This may change in the future.

To fix, consult the docs linked above for valid glob syntax.

## 1015

The glob pattern for `workspace.root` matched no paths.

This means Millet cannot determine what the workspace root(s) should be.

To fix, change the pattern to match at least one path, or create at least one file that would be matched by the pattern.

## 1016

There was a `funsig` export kind in a SML/NJ CM file.

Millet does not support `funsig` exports because it does not support `funsig` in SML source files.

## 1017

An export was undefined.

Millet uses either SML/NJ CM files or ML Basis files (aka "group" files) to know what SML source files to analyze, and in what order. Both of these group file types allow for listing "exports", which are generally the names of structures, signatures, or functors.

This error will be emitted when a group file lists an export not defined by the source files.

To fix, define the export in source files, or remove the export in the group file.

## 1018

Millet couldn't initialize the workspace root.

When the Millet language server starts up, the client. i.e. the editor, e.g. VS Code, sends the server an initialization message containing a file URL of the currently open folder, if there is one. This is the workspace root URL.

Millet will attempt to parse this URL into a real directory and process the files inside it. If parsing the URL or opening the directory fails, Millet may emit this error.

To fix, inspect the workspace root URL/underlying error message for more details.

## 1019

When run as a CLI, there was an invalid or missing argument or option.

To fix, run `--help` to see permitted options and arguments.

## 1020

A filesystem path had invalid UTF-8.

In general, paths on most modern systems can be almost arbitrary byte sequences, save for treating path separators like `/` specially and disallowing NUL bytes. However, in various places in Millet, we depend on paths being valid UTF-8.

To fix, rename the implicated file to a valid UTF-8 filename.

## 2001

There was an invalid character in the source file.

```sml
val 空条承太郎 = 3
(** ^ invalid source character *)
```

Only certain ASCII characters may appear in names and the like.

To fix, only use allowed source characters. Only ASCII characters (but not all ASCII characters) are allowed.

```sml
val kujoJotaro = 3
```

## 2002

There was an unclosed comment. This means an open comment delimiter `(*` was not matched by a later close comment delimiter `*)`.

<!-- @ignore can't point at an unclosed comment with a comment -->

```sml
val kujo = 3
(* a comment that doesn't end
val josuke = 4
```

To fix, close the comment with `*)`. Note that comments may be nested.

```sml
val kujo = 3
(* a comment that ends *)
val josuke = 4
```

## 2003

A type variable name was incomplete.

```sml
val xs : ' list = []
(**      ^ incomplete type variable *)
```

To be complete, a type variable name must be the following in sequence:

1. One or more ticks: `'`
1. An alphabetic character: `A-Z` or `a-z`
1. Zero or more alphanumeric characters or ticks: `A-Z`, `a-z`, `0-9`, or `'`

To fix, use a valid type variable name.

```sml
val xs : 'a list = []
```

## 2004

A `string` literal was not closed. String literals start and end with `"`.

<!-- @ignore too hard to point at the end of the file -->

```sml
val greeting = "hello there
(**                        ^ unclosed string literal *)
```

To fix, close the string literal with `"`.

```sml
val greeting = "hello there"
```

This error may occur when trying to embed `"` in a string literal. To embed `"` in a string literal, use `\"`.

```sml
val greeting = "he jumped down and said \"hello there\" to the general."
```

## 2005

A `word` literal was negative. Words cannot be negative.

```sml
val neg = ~0w123
(**       ^^^^^^ negative word literal *)
```

To fix, use a different type, like `int`, or remove the negative sign.

```sml
val negInt = ~123
val posWord = 0w123
```

## 2006

A `char` literal contained more (or less) than 1 character.

```sml
val tooBig = #"hello there"
(**          ^^^^^^^^^^^^^^ character literal must have length 1 *)
val tooSmall = #""
(**            ^^^ character literal must have length 1 *)
```

To fix, make the character literal 1 character long, or use a string literal.

```sml
val justRight = #"h"
val greeting = "hello there"
val empty = ""
```

## 2007

A number (`int`, `word`, or `real`) literal was incomplete. For instance, a word literal that starts with `0w`, but then with no digits following, is incomplete. Or a real literal that has no digits after the decimal point, marked with `.`, or exponent, marked with `e` or `E`.

```sml
val x : word = 0w
(**            ^^ missing digits in number literal *)
val y : real = 1.
(**            ^^ missing digits in number literal *)
val z : real = 1e
(**            ^^ missing digits in number literal *)
```

To fix, add some digits.

```sml
val x : word = 0w123
val y : real = 1.123
val z : real = 1e123
```

## 2008

A string escape was invalid.

```sml
val s = "this doesn't work: \c"
(**     ^^^^^^^^^^^^^^^^^^^^^ invalid string escape *)
```

String escapes start with `\`. Valid simple escapes are:

| Escape | Meaning         |
| ------ | --------------- |
| `\a`   | Alert           |
| `\b`   | Backspace       |
| `\t`   | Horizontal tab  |
| `\n`   | Newline         |
| `\v`   | Vertical tab    |
| `\f`   | Form feed       |
| `\r`   | Carriage return |
| `\"`   | `"`             |
| `\\`   | `\`             |

Valid complex escapes are:

- `\^C`, where `C` is a character in the ASCII range 64 (`A`) to 95 (`_`). This is useful for ASCII control characters, like `\^[`.
- `\DDD`, where each `D` is a decimal digit: `0-9`.
- `\uXXXX`, where each `X` is a hexadecimal digit: `0-9`, `A-F`, or `a-f`.

To fix, only use valid escapes.

```sml
val s = "this has\na newline"
```

## 2009

There was a non-whitespace character in a string continuation.

<!-- @ignore too hard to point at the string continuations across lines -->

```sml
val s =
  "this string is\  not
  \ valid because there\ are
  \ non-whitespace\ characters
  \ in the continuations"
```

String literals permit the sequence `\...\`, where `...` represents 1 or more whitespace characters. The sequence is ignored. We dub such sequences "string continuations", since they are often used to "continue" strings across lines.

To fix, ensure the string continuations contain only whitespace. Millet recognizes all characters defined in the Definition as whitespace, as well as some others, like carriage return (common on Windows).

```sml
val s =
  "this string is\
  \ valid because there are only\
  \ whitespace characters\
  \ in the continuations"
```

## 3001

A name that was not declared infix was used as infix.

```sml
datatype t = C of int * int
fun add (a C b) = a + b
(**          ^ non-infix name used as infix *)
```

To fix, use the name as non-infix, or declare the name as infix.

```sml
datatype t = C of int * int
fun add (C (a, b)) = a + b
infix C
fun mul (a C b) = a * b
```

## 3002

A name that was declared infix was used as non-infix without the required preceding `op` keyword.

```sml
val _ = + (2, 3)
(**     ^ infix name used as non-infix without `op` *)
```

To fix, use the name infix, or add `op`.

```sml
val _ = 2 + 3
val _ = op+ (2, 3)
```

## 3003

A fixity declaration was invalid.

This can happen when the fixity is too large.

```sml
infix 123456789123456789 foo
(**   ^^^^^^^^^^^^^^^^^^ invalid fixity: number too large to fit in target type *)
```

To fix, only use small-ish fixities.

```sml
infix 9 foo
```

## 3004

A fixity declaration was negative.

```sml
infix ~3 foo
(**   ^^ fixity is negative *)
```

To fix, only use non-negative fixities. Zero is allowed. In fact, zero is implied when a fixity number is not given.

```sml
infix 3 foo
infix 0 bar
infix quz
```

## 3005

Consecutive infix names with the same fixity, but different associativity, were used without parentheses to disambiguate.

```sml
infix <<
infixr >>
fun a << b = a + b
fun a >> b = a * b
val _ = 1 << 2 >> 3
(**            ^^ consecutive infix names with same fixity but different associativity *)
```

It's not clear if this should be parsed as `(1 << 2) >> 3` or `1 << (2 >> 3)`.

To fix, do one of the following:

- Add parentheses to disambiguate.
- Use `op` to disambiguate.
- Use different fixities.
- Use the same associativity.

## 3006

The parser expected something, but it didn't find it. For example, in this case, the parser expected a name after the `structure` keyword, but found the `val` keyword instead.

```sml
structure val platinum = 3
(**       ^^^ expected a name *)
```

This is probably the most common kind of parse error.

One bit of advice is this: Since the parser tries to continue parsing a file even in the face of errors, it may find further errors after the first one. But these errors may be all ultimately because of that first error. So, try looking at the first error in the file first.

## 3007

There was an unnecessary usage of `op`.

```sml
exception op E
(**       ^^ unnecessary `op` *)
val op x = 3
(** ^^ unnecessary `op` *)
```

To fix, remove the `op`.

```sml
exception E
val x = 3
```

## 3008

There was a unmatched closing delimiter, like `)` or `end`.

```sml
val oops = 3]
(**         ^ unmatched closing delimiter *)
```

To fix, remove the stray delimiter or match it with an opening delimiter.

```sml
val remove = 3
val addOpening = [3]
```

## 3009

A sub-expression was missing required parentheses.

```sml
val seven = 3 + if 5 > 2 then 4 else 1
(**             ^^ parentheses required around `if` expressions here *)
```

To fix, add parentheses.

```sml
val seven = 3 + (if 5 > 2 then 4 else 1)
```

## 4001

In a `fun` binding with multiple cases, the cases did not all name the same function.

```sml
fun jonathan 1 = 2
  | dio _ = 3
(** ^^^ expected a function clause for jonathan, found one for dio *)
```

To fix, use a consistent name for the function.

```sml
fun jonathan 1 = 2
  | jonathan _ = 3
```

## 4002

In a `fun` binding with multiple cases, the cases did not all have the same number of patterns.

```sml
fun muska 1 = 2
  | muska x y z = x + y + z
(** ^^^^^^^^^^^^^^^^^^^^^^^ expected 1 pattern, found 3 *)
```

To fix, use a consistent number of patterns across all cases.

```sml
fun muska 1 = 2
  | muska x = x + 3
```

## 4003

An integer (`int` or `word`) literal was invalid. This can happen when it is too large.

```sml
val n = 0w123456789123456789123456789
(**     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ invalid literal: number too large to fit in target type *)
```

To fix, use smaller literals.

```sml
val n = 0w123456789
```

## 4004

A real literal was invalid.

**NOTE:** This error is probably never emitted.

## 4005

A numeric label (as for a record) was invalid. This can happen when it was non-positive (i.e. negative or zero), or too large.

```sml
val x = { 123456789123456789123456789 = "hi" }
(**       ^^^^^^^^^^^^^^^^^^^^^^^^^^^ invalid numeric label: number too large to fit in target type *)
```

To fix, use small positive numbers for labels.

```sml
val x = { 3 = "hi" }
```

## 4006

There were multiple `...` rest pattern rows.

```sml
val {a, ..., ...} = {a = 1, b = "hi"}
(** ^^^^^^^^^^^^^ multiple `...` *)
```

To fix, only provide one such row.

```sml
val {a, ...} = {a = 1, b = "hi"}
```

## 4007

There was a non-`...` pattern row after a `...` pattern row.

```sml
val {..., b} = {a = 1, b = "hi"}
(** ^^^^^^^^ `...` must come last *)
```

To fix, put the `...` pattern row last.

```sml
val {b, ...} = {a = 1, b = "hi"}
```

## 4008

There was a bar (aka `|`) before the first:

- Case in a `fun` declaration.
- Case in a `fn`, `case`, or `handle` expression.
- Constructor in a `datatype` declaration or case.

```sml
    datatype d =
    | Chihiro
(** ^ preceding `|` *)
    | Sheeta
```

To fix, remove the bar.

```sml
datatype d =
  Chihiro
| Sheeta
```

## 4009

There was an `open` or `include` without operands.

```sml
    open
(** ^^^^ requires at least 1 operand *)
```

To fix, give them some operands, or delete the empty `open` or `include`.

```sml
structure S = struct
  val x = 3
end

open S
```

## 4010

A structure-level declaration, i.e. one that starts with one of:

- `structure`
- `signature`
- `functor`

occurred in a disallowed position, like inside a regular declaration.

```sml
val s =
  let
    structure Integer = Int
(** ^^^^^^^^^^^^^^^^^^^^^^^ structure-level declaration not allowed here *)
    val x = 3
  in
    Integer.toString x
  end
```

To fix, move the declaration to an allowed position.

```sml
structure Integer = Int
val s =
  let
    val x = 3
  in
    Integer.toString x
  end
```

This error is also emitted for `include` specifications occurring in declaration position. To fix, move or remove the `include`.

## 4011

`op` does not work with `andalso` or `orelse`.

```sml
fun bigAnd bs = List.foldl (op andalso) true bs
(**                         ^^^^^^^^^^ `andalso` and `orelse` not allowed with `op` *)
fun bigOr bs = List.foldl (op orelse) false bs
(**                        ^^^^^^^^^ `andalso` and `orelse` not allowed with `op` *)
```

`andalso` and `orelse` are SML keywords, and short-circuit. They are not infix identifiers. Because of this, they do not work with `op`.

To fix, use a lambda or helper function.

```sml
infix && ||
fun (a && b) = a andalso b
fun (a || b) = a orelse b
fun bigAnd bs = List.foldl (op &&) true bs
fun bigOr bs = List.foldl (op ||) false bs
```

## 4012

An expression was found where a declaration was expected.

In many other programming languages, constructs similar to `if` and `case` (like `switch`) are statements. SML does not have statements, and `if` and `case` are instead expressions.

In e.g. Python, you might write something like this:

```py
def foo(x):
    y = 3
    if x == 4:
        y = 5
    return y + x
```

An attempt to translate this literally into SML will emit an error:

```sml
fun foo x =
  let
    val y = 3
    if x = 4 then y = 5 else ()
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^ expression not allowed here *)
  in
    y + x
  end
```

Instead, we can define `y` as the result of evaluating an `if` expression.

```sml
fun foo x =
  let
    val y = if x = 4 then 5 else 3
  in
    y + x
  end
```

In fact, you can do this in Python as well.

```py
def foo(x):
    y = 3 if x == 4 else 5
    return y + x
```

This error may also arise when the intent is to generate a side-effect. For instance, in Python, you can write:

```py
def bar(x):
    y = x + 1
    print(y)
    return y
```

Again, in SML, an attempt to translate this literally may result in an invalid program, even when adding the necessary type conversion and newline for SML's `print`:

<!-- @ignore requires the use of semicolons -->

```sml
fun bar x =
  let
    val y = x + 1;
    print (Int.toString y ^ "\n");
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expression not allowed here *)
  in
    y
  end
```

Here, the solution could either be to:

- Use a val binding in the `let ... in`:

  ```sml
  fun bar x =
    let
      val y = x + 1
      val () = print (Int.toString y ^ "\n")
    in
      y
    end
  ```

- Use an expression sequence with `;` in the `in ... end`:

  ```sml
  fun bar x =
    let
      val y = x + 1
    in
      print (Int.toString y ^ "\n");
      y
    end
  ```

## 4013

A specification used syntax only available in declarations.

```sml
signature MAPPABLE = sig
  type 'a t
  val ('a, 'b) map : ('a -> 'b) -> ('a t -> 'b t)
(**   ^^^^^^^^ specification uses declaration syntax not allowed here *)
end
```

Some examples of declaration-only syntax:

- Explicit type variable sequences, as above.
- `op` and `rec`.
- Patterns other than `name : ty`.

To fix, remove the disallowed syntax.

```sml
signature MAPPABLE = sig
  type 'a t
  val map : ('a -> 'b) -> ('a t -> 'b t)
end
```

## 4014

There were unnecessary parentheses around something.

```sml
val n = (3)
(**     ^^^ unnecessary parentheses *)
fun inc (x) = x + 1
(**     ^^^ unnecessary parentheses *)
type t = (int)
(**      ^^^^^ unnecessary parentheses *)
```

Many things are "atomic", meaning they do not need parentheses around them to override precedence.

| Example         | Atomic? |
| --------------- | ------- |
| `false`         | Yes     |
| `(1, "hi")`     | Yes     |
| `[1, 3]`        | Yes     |
| `1 + 2`         | No      |
| `print "hi"`    | No      |
| `fn x => x + 1` | No      |

Note that e.g. `op +` is technically atomic, but this error is not issued for parentheses around it, because using parentheses around usages of `op` is somewhat idiomatic.

To fix, remove the parentheses.

```sml
val n = 3
fun inc x = x + 1
type t = int
```

## 4015

There was an overly complex expression involving `bool`s.

```sml
fun booleanIdentity x =
  if x then true else false
(** + overly complex `bool` expression *)
```

An expression is "overly complex" if it involves `if`, `andalso`, or `orelse`, and contains a `bool` literal `true` or `false`. Such expressions can always be simplified. For example:

| Complex                     | Simple            |
| --------------------------- | ----------------- |
| `if x then true else false` | `x`               |
| `if x then false else true` | `not x`           |
| `if x then y else false`    | `x andalso y`     |
| `if x then y else true`     | `not x orelse y`  |
| `if x then true else y`     | `x orelse y`      |
| `if x then false else y`    | `not x andalso y` |
| `if true then x else y`     | `x`               |
| `if false then x else y`    | `y`               |
| `x orelse true`             | `(x; true)`       |
| `x orelse false`            | `x`               |
| `x andalso true`            | `x`               |
| `x andalso false`           | `(x; false)`      |
| `true orelse x`             | `true`            |
| `false orelse x`            | `x`               |
| `true andalso x`            | `x`               |
| `false andalso x`           | `false`           |

Note also for any `b`, `(x; b)` can be further simplified to `b` if evaluating `x` has no side effects.

To fix, simplify the expression.

## 4016

There was a `case` expression with only one arm.

```sml
datatype d = D of int
fun toInt x = case x of D y => y
(**           ^^^^^^^^^^^^^^^^^^ `case` with only one arm *)
```

- An exhaustive case with only one arm is better expressed as an irrefutable binding, e.g. with `val`.
- A non-exhaustive case with only one arm should be made exhaustive.

To fix, rewrite the `case` as something else, or add more arms.

```sml
datatype d = D of int
fun toInt (D y) = y
```

## 4017

There was an unnecessary semicolon.

```sml
val x = 3;
(**      ^ unnecessary `;` *)
val y = "hi";
(**         ^ unnecessary `;` *)
```

Semicolons are used in a REPL setting to indicate the end of input, but are unnecessary in most cases in source files.

To fix, remove the semicolon.

```sml
val x = 3
val y = "hi"
```

## 4018

There were multiple type annotations on a single overall pattern.

```sml
fun add ((x : int, y : int) : int * int) = x + y
(**      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ multiple types on one pattern *)
```

This error will occur in situations like this:

```sml
val inc =
  fn (x : int) : int => x + 1
(**  ^^^^^^^^^^^^^^^ multiple types on one pattern *)
```

This may look like it is annotating both the input and output types of this `fn` as `int`, but actually it is annotating the input as `int` twice, redundantly.

Indeed, the below similar code triggers this error, as well as another error that `bool` and `int` are incompatible types. This gives a clue as to what is happening: we are actually trying to annotate `x` as both `bool` and `int`.

```sml
val greaterThanFive =
  fn (x : int) : bool => x > 5
(**  ^^^^^^^^^^^^^^^^ multiple types on one pattern *)
```

To fix, use only one type annotation. In the first example, either of the following would work:

```sml
fun addAnnotateEach (x : int, y : int) = x + y
fun addAnnotatePair ((x, y) : int * int) = x + y
```

For the `fn` examples, it may work to:

- Annotate the type elsewhere, e.g. with a `val` annotation:

  ```sml
  val inc : int -> int = fn x => x + 1
  ```

- Annotate the body of the function:

  ```sml
  val inc = fn (x : int) => (x + 1) : int
  ```

- Skip annotating the return type:

  ```sml
  val inc = fn (x : int) => x + 1
  ```

## 4019

There was a declaration missing its right-hand side.

```sml
val x : int
(** ^^^^^^^ missing right-hand side of declaration *)
```

The above example is valid specification syntax for signatures, but is not a valid declaration.

To fix, provide a right-hand side.

```sml
val x : int = 3
```

## 4020

There was a declaration with a `sharing type`.

```sml
val x = 3 sharing type t = u
(**       ^^^^^^^^^^^^^^^^^^ `sharing type` not allowed here *)
```

`sharing type` is allowed on specifications, not declarations.

To fix, remove the `sharing type`.

## 4021

There was a declaration with `eqtype`.

```sml
eqtype num = int
(** + `eqtype` not allowed here *)
```

`eqtype` is allowed on specifications, not declarations.

To fix, change `eqtype` to `type`.

## 4022

There was a declaration hole.

```sml
structure S = struct ... end
(**                  ^^^ declaration hole *)
```

Declaration holes are written `...`.

Sometimes `...` is used in declaration position as a placeholder or filler in examples.

To fix, replace or remove the hole.

## 4023

There was a non-specification declaration in a specification context.

```sml
signature S = sig
fun inc x = x + 1
(** + non-specification not allowed here *)
end
```

To fix, move the declaration out of the signature, or remove it.

## 4024

The left-hand side of an `as` pattern was neither a name nor a typed name.

```sml
fun f x =
  case x of
    3 as y => y
(** ^ left-hand side of `as` pattern must be a name *)
  | _ => x
```

The syntax for `as` patterns is `<name> (: <ty>)? as <pat>`.

To fix, ensure the left hand side of the `as` is either a name or a typed name.

```sml
fun f x =
  case x of
    y as 3 => y
  | _ => x
```

## 4025

A name bound in a pattern in a matcher (e.g. `case`) was the same as the name of a `fun` declaration that contained the pattern.

```sml
fun foo x =
  case x of
    0 => 1
  | foo => foo
(** ^^^ name bound in pattern inside a `case` matches name of a `fun` that contains the `case` *)
```

This is at best a possibly confusing case of shadowing.

This warning occurs in the following somewhat common scenario:

<!-- @ignore many errors and warnings, including the one mentioned -->

```sml
fun foo 0 y = y
  | foo 1 y =
      case y of
        0 => 1
      | 2 => 3
      | _ => 4
  | foo x y = x + y
(** ^^^ name bound in pattern inside a `case` matches name of a `fun` that contains the `case` *)
```

This looks like a `fun` with many cases, one of which has an inner `case`. However, most SML parsers (including Millet) attempt to parse the final `foo x y` as part of the `case` instead as part of the `fun`. This leads to confusing errors, often [3001](#3001) and [3002](#3002).

To fix, try one of the following:

- Put parentheses around the inner matcher (i.e. `case`, `fn`, or `handle`).
- Rename the binding to avoid shadowing.

```sml
fun foo 0 y = y
  | foo 1 y =
      (case y of
        0 => 1
      | 2 => 3
      | _ => 4)
  | foo x y = x + y
```

## 4026

There was a `fun` with no parameters.

```sml
fun totoro = 3
(** + requires at least 1 parameter *)
```

To fix, add some parameters.

```sml
fun totoro x = x + 3
```

## 4027

There was an expression sequence with no expressions.

```sml
val _ = let in end
(** + requires at least 1 expression *)
```

To fix, add some expressions.

```sml
val a = let in 1 end
val b = let in 1; 2; 3 end
```

## 4028

There was a trailing comma or semicolon.

```sml
val x = (1, 2,)
(**          ^ trailing `,` *)
```

To fix, remove the trailing separator.

```sml
val x = (1, 2)
```

## 4999

There was an occurrence of an unsupported SML construct.

```sml
val x = #[1, 2]
(**     ^^^^^^^ unsupported: vector expressions *)
```

At time of writing, Millet does not support the following constructs:

| Name                         | Example                   |
| ---------------------------- | ------------------------- |
| Vector expressions           | `val _ = #[1, 2]`         |
| Vector patterns              | `fn #[1, 2] => 3`         |
| Do declarations              | `do print "hello"`        |
| Expression row punning       | `val _ = {a, b}`          |
| `withtype` in specifications | (same as in declarations) |

Note that these constructs are not defined by the Definition, but are somewhat common extensions in implementations like SML/NJ and MLton.

To fix, avoid such constructs.

## 5001

A name was referenced, but it was not defined in that scope.

```sml
val bar = foo
(**       ^^^ undefined value: foo *)
```

To fix, try any of the following:

- Check that the name is correctly spelled.
- Check the if the name is defined in the current scope unqualified, or if it is in a structure. For instance, `filter` is defined in `structure List`, not at the top level. Some functions like `map` are defined both in `List` and at the top level.
- Check the error message to see what kind of thing was not defined: value, type, structure, etc. These different kinds of items have different namespaces.

  In this example, there is a value named `x` defined, but then we try to use `x` as a type. There is no type `x` defined, so this is invalid.

  ```sml
  val x = 4
  val y : x = 5
  (**     ^ undefined type: x *)
  ```

- Check that the name is not explicitly forbidden from being accessed by a signature ascription. Ascribing a structure to a more restrictive signature prohibits accessing the "extra" items inside the structure.

  ```sml
  signature SIG = sig
    val foo : int
  end

  structure Str : SIG = struct
    val foo = 3
    val bar = "hi"
  end

  val s = Str.bar
  (**     ^^^^^^^ undefined value: bar *)
  ```

## 5002

There was a duplicate of something.

This may occur when using `and` to declare many things at once.

```sml
val x = 3
and x = 4
(** ^ duplicate value: x *)
```

It may also occur when binding the same name more than once in a pattern.

```sml
fun add (x, x) = x + x
(**         ^ duplicate value: x *)
```

To fix, use different names, or avoid `and`. (The latter induces shadowing.)

```sml
val x = 3
val x = 4
```

## 5003

Something was requested by a signature, but not present in the structure that is attempting to ascribe to that signature.

```sml
signature SIG = sig
  val x : int
  val y : string
end

structure Str : SIG = struct
(**                   ^^^^^^ missing value required by signature: x *)
  val y = "oops"
end
```

To fix, provide definitions for the missing items.

See also [5034](#5034) for a particular case in which this error may confusingly appear.

## 5004

Something was not requested by a signature, but was present in the structure that is attempting to ascribe to that signature.

Usually, this is allowed, but it is forbidden for `datatype` declarations.

```sml
structure S
  : sig    datatype d = Pazu          end
  = struct datatype d = Pazu | Sosuke end
(** ^^^^^^ extra value not present in signature: Sosuke *)
```

To fix, ensure only the requested items are defined.

```sml
structure S
  : sig    datatype d = Pazu end
  = struct datatype d = Pazu end
```

## 5005

Typechecking failed, because of "circularity", which means we attempted to a set a type variable to be equal to a type containing that type variable itself.

Consider this example:

```sml
fun f x = x x
(**       ^ circular type: ?a occurs in ?a -> ?b *)
```

When typechecking `f`, Millet does the following:

1. Conjure up a fresh, unconstrained type variable for the name `x`. Let's use `?x` as the type variable for `x`.
2. Enter the body of `f` to see how `x` is used, noting constraints on `?x` as we go.

Entering the body of `f`, we see the entire body is one application expression: `x x`, the application of `x` to itself.

- `x` is used as a function, so it must have a type like `?a -> ?b` where `?a` and `?b` are types. So, we have that `?x = ?a -> ?b`.
- Further, `x` is used as the argument to a function, which we just said has type `?a -> ?b`. So we have `?x = ?a`, the type of the argument to the function.

We now have

- `?x = ?a -> ?b`
- `?x = ?a`

Substituting, we have `?a = ?a -> ?b`. That is, we are setting a type variable, in this case `?a`, to a type, in this case `?a -> ?b`, that contains itself. This is not allowed.

## 5006

Two types that were supposed to be compatible were not.

This is probably the most common typechecking error.

### Expected and found

Millet tries to report which type was "expected" and which was "found". For instance, in this example, we consider `int` the "expected" type, because of the annotation. This explicit annotation implies the programmer really thought it should be that type.

```sml
val x : int = "no"
(** + expected int, found string *)
```

This hints at a possible strategy for debugging this kind of error: if the expected and found types are confusing, try adding more type annotations.

### Function application

This error commonly occurs when applying a function to an argument, but the argument did not have the type the function expected. For instance, in this example, Millet reports that we "expected" `bool`, because the function `choose` takes a `bool`.

```sml
fun choose x = if x then "sosuke" else "pazu"
val _ = choose 4
(**            ^ expected bool, found int *)
```

### Fully qualified names

Millet reports types defined in structures and signatures prefixed with the names of the relevant structures or signatures. This is sometimes called a "fully qualified name" or "FQN".

Because of internal Millet design decisions, Millet reports the type `t` in this example as `FOO.t` instead of `Foo.t`:

```sml
signature FOO = sig
  type t
  val x : t
end

structure Foo :> FOO = struct
  type t = int
  val x = 3
end

val y = Foo.x : unit
(**     ^^^^^^^^^^^^ expected unit, found FOO.t *)
```

### Overloads

Certain built-in functions, like `+`, `<`, and `abs`, are overloaded, which means they may work with a certain fixed number of types. For instance, `+` works with `int`, `word`, and `real`, while `<` works for those as well as `string` and `char`.

When using overloaded functions, there must exist a single actual type being used. For instance, `+` works with `word`, `real`, and `int`. However, `+` cannot add a `real` to a `word`, or an `int` to a `real`, or any such similar combination. It can only add two `word`s, or two `real`s, or two `int`s.

### Equality

Part of checking for type compatibility is ensuring that the equality attribute of types is respected. For instance, `=` and `<>` compare expressions whose type must be an equality type, and `real` is not an equality type, so the following fails.

```sml
val _ = 1.2 <> 3.4
(**     ^^^^^^^^^^ not an equality type *)
```

### Reporting types with invalid syntax

Millet uses pseudo-syntax that is not technically valid SML syntax to report some more exotic types.

It reports overloaded types as the following:

| Overload       | Meaning                                 |
| -------------- | --------------------------------------- |
| `<wordint>`    | `word`, `int`                           |
| `<realint>`    | `real`, `int`                           |
| `<num>`        | `word`, `real`, `int`                   |
| `<wordinttxt>` | `word`, `int`, `string`, `char`         |
| `<numtxt>`     | `word`, `real`, `int`, `string`, `char` |

It will report type variables that haven't been "solved" yet with the syntax `?a`, `?b`, etc.

Unsolved equality type variables will begin with two `?`, rather like real SML type variables, which begin with two `'` when they are equality type variables.

Unsolved record types that may have other fields, which often arise with usages of `#` selectors, will be reported as record types with an extra `...` row.

If Millet encounters an invalid expression, like a variable that was undefined, it will report the type `_`.

## 5007

**NOTE**: This diagnostic is no longer emitted.

A function application expression was encountered, but the function expression did not have a function type.

<!-- @ignore no longer emitted -->

```sml
val _ = "foo" 3
(**     ^^^^^ expected a function type, found string *)
```

In this example, we attempt to treat the string `"foo"` as a function and apply it to the argument `3`.

This error is a special case of 5006, specialized for the common case of function application.

To fix, only apply functions to arguments.

## 5008

There was a duplicate label.

```sml
val x = { a = 1, a = 2 }
(**     ^^^^^^^^^^^^^^^^ duplicate label: a *)
```

To fix, use differently named labels, or remove one of the record rows.

```sml
val x = { a = 1, b = 2 }
val x = { a = 1 }
```

## 5009

A real literal was used as a pattern.

```sml
fun f (x : real) : int =
  case x of
    1.2 => 3
(** ^^^ real literal used as a pattern *)
  | _ => 4
```

To fix, consider checking that the given real is within some epsilon value of the desired real.

```sml
val eps = 0.01

fun f (x : real) : int =
  if Real.abs (x - 1.2) <= eps then
    3
  else
    4
```

Usage of `Real.==` to check for equality between reals is discouraged, due to [limitations](https://0.30000000000000004.com) around representing floating-point (aka, `real`) numbers on most architectures.

## 5010

A pattern in a `case` expression or similar (like `handle`) was not reachable.

Patterns in a `case` are tried from top to bottom. If a pattern further up the `case` always matches certain values, then the lower pattern will never be reached. Thus, the lower pattern is unreachable.

```sml
fun f x =
  case x of
    1 => 2
  | 1 => 3
(** ^ unreachable pattern *)
  | _ => 4
```

To fix, try:

- Making the higher pattern more specific, so the lower pattern may be reached.
- Removing the lower pattern.

## 5011

A `case` expression (or similar) was not exhaustive.

When we `case` on a value of a given type, we must assume that value can be any possible value of that type.

Thus, if:

- there exists a value of the type being matched, such that
- there does _not_ exist a pattern in the `case` that matches that value,

then the `case` is not exhaustive.

```sml
datatype d = A of string | B of int | C of bool

fun f (x : d) : int =
    case x of
(** ^^^^^^^^^ non-exhaustive case: missing A _ *)
      B y => y
    | C z => if z then 1 else 2
```

To fix, add patterns matching the missing cases. The error message reports examples of patterns not matched.

## 5012

A binding, like with `val`, was not exhaustive.

This is effectively the same error as 5011, but for singular bindings.

```sml
datatype d = A of string | B of int | C of bool

fun f (x : d) : string =
  let
    val A y = x
(**     ^^^ non-exhaustive binding: missing C _, B _ *)
  in
    y
  end
```

The pattern in a `val` binding ought to be "irrefutable", to wit, it alone ought to match all possible values of the type of the expression. For example:

- The wildcard pattern `_` is irrefutable.
- Variable patterns like `x` and `y` are irrefutable.
- Tuple patterns composed of irrefutable patterns are irrefutable.
- If a `datatype` has only one constructor, then a pattern of that constructor, additionally with an irrefutable pattern argument if one is needed, is irrefutable.

To fix, use a `case` or similar instead.

## 5013

A pattern match treated a value as if it were a pattern.

```sml
structure S = struct
  val x = 3
end

fun f y =
  case y of
    S.x => 1
(** ^^^ value binding used as a pattern *)
  | 4 => 5
  | _ => 6
```

To fix check for equality another way, for instance with `=`.

```sml
structure S = struct
  val x = 3
end

fun f y =
  if y = S.x then
    1
  else if y = 4 then
    5
  else
    6
```

## 5014

A constructor had an argument in a pattern match, but it was defined to have no argument.

```sml
datatype d = A | B of int

fun f x =
  case x of
    A y => y + 1
(** ^^^ unexpected argument for constructor pattern *)
  | B z => z - 1
```

To fix, try one of the following:

- Remove the argument from the pattern:

  ```sml
  datatype d = A | B of int

  fun f x =
    case x of
      A => 1
    | B z => z - 1
  ```

- Define the constructor to have an argument:

  ```sml
  datatype d = A of int | B of int

  fun f x =
    case x of
      A y => y + 1
    | B z => z - 1
  ```

## 5015

A constructor had no argument in a pattern match, but it was defined to have an argument.

```sml
datatype d = A | B of int

fun f x =
  case x of
    A => 1
  | B => 2
(** ^ missing argument for constructor pattern *)
```

To fix, try one of the following:

- Add an argument to the pattern:

  ```sml
  datatype d = A | B of int

  fun f x =
    case x of
      A => 1
    | B _ => 2
  ```

- Define the constructor to not have an argument:

  ```sml
  datatype d = A | B

  fun f x =
    case x of
      A => 1
    | B => 2
  ```

## 5016

An invalid name was used as the left hand side of an `as` pattern.

As-patterns allow binding the entirety of a pattern `p` to a name `n` with the pattern `n as p`. However, the name `n` may not, for instance, already exist as a non-value:

```sml
exception e
fun f x =
  case x of
    e as [_] => 1 :: e
(** ^^^^^^^^ invalid `as` pat name: e *)
  | _ => []
```

To fix, use a valid name.

```sml
exception e
fun f x =
  case x of
    y as [_] => 1 :: y
  | _ => []
```

## 5017

A type escapes the scope in which it is valid.

Here, the type `d` is only available for the scope of the `let` expression, but the `let` expression would cause a value of type `d` to "escape" the `let` and be bound to `x`, outside the `let`.

```sml
val x =
    let datatype d = D in D end
(** ^^^ type escapes its scope: d *)
```

This can also occur for explicit type variables, as in this example:

```sml
fun f b x =
(** + type escapes its scope: 'a *)
  let
    fun g (y : 'a) = if b then x else y
  in
    ()
  end
```

Here, `x` and `y` are both inferred to have type `'a`. Note that:

- `'a` is implicitly bound at `fun g`.
- `x` is bound before `fun g`.

To fix, try one of the following:

- Extend the scope of the type:

  ```sml
  datatype d = D
  val x = D
  ```

- Do not allow its values to escape its scope:

  ```sml
  val x =
    let
      datatype d = D
    in
      4
    end
  ```

- Remove explicit type variable annotations:

  <!-- @ignore unused value -->

  ```sml
  fun f b x =
    let
      fun g y = if b then x else y
    in
      ()
    end
  ```

- Change where type variables are bound:

  <!-- @ignore unused value -->

  ```sml
  fun 'a f b x =
    let
      fun g (y : 'a) = if b then x else y
    in
      ()
    end
  ```

## 5018

In a `val rec` binding, the expression must be a literal `fn` expression.

```sml
val rec x = x + 3
(** + the expression for a `val rec` was not a `fn` *)
```

It is an error even if the expression does not use the recursive binding.

```sml
val rec x = 3
(** + the expression for a `val rec` was not a `fn` *)
```

It is also an error even if the expression has function type.

```sml
val mkAdd3 = fn () => fn x => x + 3
val rec add3 = mkAdd3 ()
(** + the expression for a `val rec` was not a `fn` *)
```

To fix, ensure the expression is a literal `fn` expression.

```sml
val rec add3 = fn n => n + 3
val rec fact = fn n => if n = 0 then 1 else n * fact (n - 1)
```

## 5019

The wrong number of type arguments was passed to a type-level function.

```sml
type ('a, 'b) pair = 'a * 'b
type nope = int pair
(**         ^^^^^^^^ expected 2 type arguments, found 1 *)
```

`datatype`s, like `'a list`, also define type-level functions.

```sml
val xs : list = []
(**      ^^^^ expected 1 type argument, found 0 *)
```

To fix, pass the correct number of type arguments.

```sml
type ('a, 'b) pair = 'a * 'b
type yep = (int, string) pair
val xs : yep list = []
```

## 5020

In an exception copy declaration, the right hand side was not an exception.

```sml
val e = 3
exception Nope = e
(** + not an exception: e *)
```

To fix, only use exceptions on the right hand side.

```sml
exception E
exception Nope = E
```

## 5021

Certain names in certain namespaces may not be rebound. These names are:

| Name    | Definition                                         |
| ------- | -------------------------------------------------- |
| `true`  | logical truth                                      |
| `false` | logical falsity                                    |
| `nil`   | the empty list constructor                         |
| `::`    | the non-empty list constructor                     |
| `ref`   | the reference constructor                          |
| `=`     | the polymorphic equality function                  |
| `it`    | the value of the last expression entered in a REPL |

```sml
val it = 3
(** ^^ cannot re-bind name: it *)
```

To fix, do not attempt to rebind these names.

## 5022

Names have "statuses", which can be one of:

- exception
- constructor
- value

These statuses must be compatible for the purposes of matching a structure against a signature.

```sml
exception Foo

structure S
  : sig    exception E       end
  = struct val       E = Foo end
(** ^^^^^^ incompatible identifier statuses: E *)
```

To fix, ensure the names have compatible statuses.

```sml
exception Foo

structure S
  : sig    exception E       end
  = struct exception E = Foo end
```

## 5023

A record type couldn't be fully resolved, due to the use of a `...` pattern row with insufficient surrounding context.

```sml
fun getX {x, ...} = x
(** + cannot resolve `...` in record type: { x : ?a, ... } *)
```

SML lacks row polymorphism, so the above example function does not typecheck.

To fix, consider adding a type annotation.

```sml
type t = {x : int, y : bool, z : string}
fun getX ({x, ...} : t) = x
```

This error may arise when using `#` selectors.

```sml
fun addFooBar x = #foo x + #bar x
(** + cannot resolve `...` in record type: { bar : <num>, foo : <num>, ... } *)
```

Again, the fix is usually to add a type annotation. Though, an alternative would be to avoid `...` pattern rows altogether.

```sml
fun addFooBar {foo, bar} = foo + bar
```

## 5024

Not all or pattern alternatives bound the same names.

```sml
datatype t = Foo of int | Bar of int

fun toInt (x : t) : int =
  let val (Foo y | Bar _) = x
(**                ^^^^^ y was bound in one alternative, but not in another *)
  in y end
```

To fix, ensure all alternatives bind the same names. The types must also match.

```sml
datatype t = Foo of int | Bar of int

fun toInt (x : t) : int =
  let val (Foo y | Bar y) = x
  in y end
```

Note that or patterns are not permitted by the Definition, though they are a common extension, implemented by SML/NJ and MLton.

## 5025

A `signature` or `functor` declaration occurred in a disallowed position, like inside `struct ... end`.

```sml
structure Str = struct
  signature SIG = sig end
(** + `signature` or `functor` not allowed here *)
  functor Func() = struct end
(** + `signature` or `functor` not allowed here *)
end
```

To fix, declare the signature or functor at the top level.

```sml
structure Str = struct end
signature SIG = sig end
functor Func() = struct end
```

Although not permitted by the Definition, Millet also allows defining the signature or functor in a `local`.

```sml
local
  signature SIG = sig val y : int end
  functor Func(val x : int) : SIG = struct val y = x + 2 end
in
  structure S4 : SIG = Func(val x = 4)
  structure S7 : SIG = Func(val x = 7)
end
```

## 5026

There was an expression hole.

```sml
val answer = if ... then "yes" else "no"
(**             ^^^ expression hole with type bool *)
```

The error message contains information about the type of the hole given the surrounding context. For instance, in the above example, the hole is reported to have type `bool` because it is being used as the condition to an `if` expression.

Expression holes can either be `...` or `_`.

```sml
fun f x = _ + 5
(**       ^ expression hole with type int *)
val ans = f 3
```

To fix, replace the hole with a real expression of the correct type.

## 5027

There was a type hole.

```sml
type thing = ... list * int
(**          ^^^ type hole *)
```

Type holes can be either `...` or `_`.

```sml
type func = int -> _
(**                ^ type hole *)
```

To fix, replace the hole with a real type.

## 5028

An attempt was made to bind an expansive expression to a variable, where that variable would then have polymorphic type.

This error is also called the "value restriction".

An expansive expression is one that may raise an exception or allocate memory during evaluation. Some examples of expansive expressions:

| Name                   | Example                       |
| ---------------------- | ----------------------------- |
| Application (see note) | `f x`                         |
| Raise                  | `raise Bad`                   |
| Handle                 | `e handle Bad => e'`          |
| Let                    | `let val x = e in x + e' end` |

Note that an application expression `f x` is considered expansive if:

- The function `f` is the `ref` constructor.
- The function `f` is a "regular" function (i.e. **not** a constructor).
- The argument `x` is expansive.

This means that e.g. `SOME 3` is **not** expansive, because the function (`SOME`) is a non-`ref` constructor, and the argument (`3`) is not expansive.

A polymorphic type is one which contains type variables, like `'a` or `'b`.

If Millet did not emit an error for cases like this, we could break type safety:

```sml
val r : 'a option ref = ref NONE
(**                     ^^^^^^^^ cannot bind expansive polymorphic expression *)
val () = r := SOME "foo"
val v : int = valOf (!r)
```

The first line is forbidden by this error. If it were not, then after the execution of the above program, `v` would have type `int`, but the value `"foo"`, which actually has type `string`.

This is a violation of type safety, namely the highly desirable property of "soundness". In the context of type checkers and type systems, soundness means that:

- If the type checker determines that an expression $e$ has a type $\tau$,
- Then at runtime, the expression $e$ will actually have that type $\tau$.

To fix, try any of the following:

- If the expression has function type, add an extra variable to both the binding site and the expression. This is called eta-expansion, the opposite of eta-reduction.

  Before:

  ```sml

  val mapFst =
    List.map (fn (x, _) => x)
  (** + cannot bind expansive polymorphic expression *)

  ```

  After:

  ```sml
  fun mapFst xs =
    List.map (fn (x, _) => x) xs
  ```

- Annotate the expression (or pattern) with a non-polymorphic type.

  Before:

  ```sml
  val r = ref []
  (**     ^^^^^^ cannot bind expansive polymorphic expression *)
  ```

  After:

  ```sml
  val r : int list ref = ref []
  ```

## 5029

There was an unused variable.

```sml
fun ignoreArg x = 3
(**           ^ unused value: x *)
```

To fix, use the variable, or do not define it.

```sml
fun useArg x = x + 3
fun doNotBindArg _ = 3
```

## 5030

A type variable bound at a `val` or `fun` declaration was used in the right-hand side of a `type` or `datatype` declaration.

```sml
fun 'a foo (x : 'a) =
  let
    type t = 'a * 'a
(**          ^^ type variable bound at `val` or `fun` not allowed here *)
  in
    (x, x) : t
  end
```

To fix, bind the type variable at the `type` or `datatype`, or remove it from the right-hand side.

```sml
fun 'a foo (x : 'a) =
  let
    type 'b t = 'b * 'b
  in
    (x, x) : 'a t
  end
```

## 5031

A `sharing type` was invalid.

```sml
signature SIG = sig
  type a = int
(**    + cannot share type a as int *)
  type b = int
  sharing type a = b
end
```

## 5032

A `where type` was invalid.

```sml
signature BAD = sig
(**             + cannot realize type t as int *)
  type t
end
  where type t = int
  where type t = string
```

This error also arises when using `type a = b` in signatures, because that is syntactic sugar for a usage of `where type`.

```sml
signature BAD = sig
(**             + cannot realize type t as int *)
  type t = int
end
  where type t = string
```

## 5033

The equality function `=` or the inequality function `<>` was applied to a discouraged argument.

```sml
fun isEmpty xs = xs = []
(**              ^^^^^^^ calling `=` or `<>` on nil *)
```

Using `=` or `<>` may unnecessarily restrict the type to be an "equality" type. Millet will report the type of `isEmpty` above as `''a list -> bool` instead of the perhaps expected `'a list -> bool`.

Note the extra tick mark: `''a` is a type variable that can only be substituted with equality types, whereas `'a` is an unrestricted type variable. This is due to the usage of `=`.

Although `bool` is already an equality type, it is also discouraged to call `=` or `<>` on boolean literals, because the expression can be simplified:

| Complex      | Simple  |
| ------------ | ------- |
| `x = true`   | `x`     |
| `x = false`  | `not x` |
| `x <> true`  | `not x` |
| `x <> false` | `x`     |

To fix, if the error is on a `bool` literal, consult the above table to simplify the expression. For lists and options, consider whether you need to access the values inside the constructors (e.g. the head or tail of the list, or the value in a `SOME`.) If you do, use pattern matching.

Before:

```sml
fun sum xs =
  if xs = [] then
(**  ^^^^^^^ calling `=` or `<>` on nil *)
    0
  else
    hd xs + sum (tl xs)

fun foo opt =
  if opt = NONE then
(**  ^^^^^^^^^^ calling `=` or `<>` on NONE *)
    "hi"
  else
    valOf opt ^ "!"
```

After:

```sml
fun sum xs =
  case xs of
    [] => 0
  | x :: r => x + sum r

fun foo opt =
  case opt of
    NONE => "hi"
  | SOME x => x ^ "!"
```

If you don't need the values inside the constructors, use the convenience functions `List.null` and `Option.isSome`.

Before:

```sml
fun reportList xs =
  if xs = [] then
(**  ^^^^^^^ calling `=` or `<>` on nil *)
    "empty list"
  else
    "non empty list"

fun reportOption opt =
  if opt = NONE then
(**  ^^^^^^^^^^ calling `=` or `<>` on NONE *)
    "nothing"
  else
    "something inside"
```

After:

```sml
fun reportList xs =
  if List.null xs then
    "empty list"
  else
    "non empty list"

fun reportOption opt =
  if Option.isSome opt then
    "something inside"
  else
    "nothing"
```

## 5034

A functor application did not use the declaration "syntax sugar" when the functor definition did, or vice versa.

This example triggers the warning:

```sml
functor Func (val x : int) = struct end
structure S = Func (struct val x = 3 end)
(**           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ the functor definition uses syntax sugar, but the functor application does not *)
```

This example triggers a warning, and also other errors:

<!-- @ignore declaration hole -->

```sml
signature SIG = sig type t end
functor Func (structure Param : SIG) = struct ... end
structure Arg : SIG = struct type t = unit end
structure S = Func (Arg)
```

This is invalid, but the reason why is subtle. Before we explain the reason, note that the bottom line is that the call site of `Func` must be changed:

```diff
-structure S = Func (Arg)
+structure S = Func (structure Param = Arg)
```

The fact that both the definition site and the corrected call site for `Func` have an extra `structure` keyword is a clue.

The key is in the definition of `Func`. We use the syntax:

<!-- @ignore invalid syntax -->

```sml
functor Func (structure Param : SIG)
```

which has a distinct meaning from:

<!-- @ignore invalid syntax -->

```sml
functor Func (Param : SIG)
```

Both forms are legal SML. The first form is syntax sugar. This means it is extra "helper" syntax that is fully defined in terms of the second, more "fundamental" syntax.

In the example, the sugar is expanded in the following manner:

<!-- @ignore invalid syntax -->

```sml
(* original *)
functor Func (structure Param : SIG) = struct ... end

(* desugared *)
functor Func (<<FuncArg>> : sig
  structure Param : SIG
end) = let
  open <<FuncArg>>
in
  struct ... end
end
```

Note the invalid SML syntax for the name of the functor argument `<<FuncArg>>`. This is to emphasize that when expanding the syntax sugar, an SML implementation will generate an unnameable, unique structure name. This name is then used exclusively in the `open`.

Similarly, once we modify the call site, we are using more syntax sugar, which also expands:

<!-- @ignore syntax sugar example -->

```sml
(* original *)
structure S = Func (structure Param = Arg)

(* desugared *)
structure S = Func (struct structure Param = Arg end)
```

A similar but "opposite" error may occur if the definition site does not use the syntax sugar, but the call site does. As in:

<!-- @ignore declaration hole -->

```sml
signature SIG = sig type t end
functor Func (Param : SIG) = struct ... end
structure Arg : SIG = struct type t = unit end
structure S = Func (structure Param = Arg)
```

This will error. To fix, do not use the syntax sugar at the call site.

```diff
-structure S = Func (structure Param = Arg)
+structure S = Func (Arg)
```

## 5035

There was a call to `@`, the list append function, with a discouraged first argument.

<!-- @ignore this check does not work with mini std basis -->

```sml
fun overlyComplicatedId xs =
  [] @ xs
(** + calling `@` with an empty list *)

fun overlyComplicatedId' xs =
  xs @ []
(** + calling `@` with an empty list *)

fun overlyComplicatedCons x xs =
  [x] @ xs
(** + calling `@` with a singleton list *)
```

These expressions can be simplified:

| Complex    | Simple    |
| ---------- | --------- |
| `[] @ xs`  | `xs`      |
| `xs @ []`  | `xs`      |
| `[x] @ xs` | `x :: xs` |

To fix, simplify the expressions.

## 5036

There was a `case` on a `bool` expression.

```sml
fun mainCharacter old =
    case old of
(** ^^^^^^^^^^^ `case` on a `bool` *)
      true => "porco rosso"
    | false => "nausicaa"
```

To fix, use `if` instead.

```sml
fun mainCharacter old =
  if old then
    "porco rosso"
  else
    "nausicaa"
```

## 5037

A function literal expression ("lambda") was applied to an argument.

```sml
val _ = (fn x => x + 1) 3
(**     ^^^^^^^^^^^^^^^^^ applying a function literal to an argument *)
```

To fix, "inline" the literal, or use something like `let` or `case`.

```sml
val inlined = 3 + 1

val usingLet =
  let
    val x = 3
  in
    x + 1
  end
```

## 5038

There was a usage of the top-level `use` function, which has implementation-defined semantics.

```sml
val () = use "foo.sml"
(**      ^^^^^^^^^^^^^ `use` ignored *)
val x = Foo.bar + 3
```

This function is provided by some SML implementations, like SML/NJ, but not others, like MLton. When it is provided, its approximate semantics are to "load" the contents of the SML file with the given name, bringing all of its definitions into scope in the file that called `use`.

This is sometimes used for simple multi-file SML projects. However, Millet only supports using "group files", aka ML Basis and SML/NJ CM, to coordinate multi-file SML projects.

To fix, use group files to inform Millet about the dependencies between SML files.

If you wish to also compile and run your SML project with group files, you can remove the calls to `use`. If, however, you only want to use group files for Millet, and still want to use `use` when compiling your project, you can ignore this error in your `millet.toml`.

## 5039

An expression that will never `raise` was surrounded with a `handle`. This means none of the arms of the `handle` will be reachable.

```sml
val a = 4 handle Overflow => 5
(**     ^^^^^^^^^^^^^^^^^^^^^^ unreachable `handle` *)
```

To fix, make the handled expression actually (possibly) raise something, or remove the `handle`.

```sml
val b = (4 + 123123123) handle Overflow => 5
val c = 4
```

## 5999

There was an occurrence of an unsupported SML construct.

```sml
abstype t = T with val _ = 3 end
(** + unsupported: `abstype` declarations *)
```

At time of writing, Millet does not support `abstype` declarations.

To fix, avoid such constructs. `abstype` is often avoided in modern SML, replaced with a combination of `datatype`, `structure`, and `signature`.

## 6001

A comment prevented automatic formatting of a SML file.

To fix, try one of the following:

- Move, merge, or remove the comment.
- Disable automatic formatting.

[config]: /docs/manual.md#configuration
