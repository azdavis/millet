# Errors

This is documentation for all the errors Millet can emit, and what they mean.

If Millet emitted an error not documented here, please file an issue.

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

There were multiple root group files (aka SML/NJ CM or ML Basis files).

Given a directory to serve as the root, Millet will look for a **single** `.cm` or `.mlb` file directly in that directory. (By "directly", we mean that Millet will not recurse into subdirectories to do this.)

If exactly one such file is found, it is treated as the root group file. But if more than one was found, Millet requires that you disambiguate with a config file.

To fix, either remove all but one of the root group files, or select which one you want to be the root group file with a `millet.toml` [config file][config], like this:

```toml
version = 1
workspace.root = "foo.cm"
```

## 1004

There was no group file (aka `.mlb` or `.cm` file) in the top-level (aka "root") directory.

See error 1003, which is the error when there was more than one root group file.

To fix, try any of the following:

- Create a group file in the top-level directory.
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

To fix, use only the subset of CM syntax Millet understands (which is, at time of writing, very limited).

## 1009

There was an error when parsing a ML Basis file.

To fix, use only the subset of MLB syntax Millet understands. Some features, like string paths, are not supported.

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

In a `millet.toml` config file, a key of the `errors` table was not a valid error code.

A valid error code will be a positive integer.

To fix, make the key of the `errors` table an error code.

## 1013

In a `millet.toml` config file, both `workspace.members` and another configuration setting were set.

<!--

When a `millet.toml` file has `workspace.members` set, the `members` are subdirectories that themselves have `millet.toml` files in them. This allows for a single project to have many different sub-projects.

But, when a `millet.toml` has the other workspace properties set, it may not contain sub-projects.

 -->

To fix, do not set any other configuration (other than the required `version`) when setting `workspace.members` in a `millet.toml` file.

## 1996

Millet couldn't initialize the workspace root.

When the Millet language server starts up, the client. i.e. the editor, e.g. VS Code, sends the server an initialization message containing a file URL of the currently open folder, if there is one. This is the workspace root URL.

Millet will attempt to parse this URL into a real directory and process the files inside it. If parsing the URL or opening the directory fails, Millet may emit this error.

To fix, inspect the workspace root URL/underlying error message for more details.

## 1997

When run as a CLI, there was an invalid or missing argument or option.

To fix, run `--help` to see permitted options and arguments.

## 1998

An export was undefined.

Millet uses either SML/NJ CM files or ML Basis files (aka "group" files) to know what SML source files to analyze, and in what order. Both of these group file types allow for listing "exports", which are generally the names of structures, signatures, or functors.

This error will be emitted when a group file lists an export not defined by the source files.

To fix, define the export in source files, or remove the export in the group file.

## 1999

There was an unsupported export kind in a SML/NJ CM file.

At time of writing, Millet does not support:

- `funsig` exports.
- `source(s)` and `source(-)` exports.
- `group(g)` exports.

## 2001

There was an invalid character in the source file.

```sml
(* error *)
val 空条承太郎 = 3
```

Only certain ASCII characters may appear in names and the like.

To fix, only use allowed source characters. Only ASCII characters (but not all ASCII characters) are allowed.

```sml
(* ok *)
val kujoJotaro = 3
```

## 2002

There was an unclosed comment. This means an open comment delimiter `(*` was not matched by a later close comment delimiter `*)`.

```sml
(* error *)
val kujo = 3
(* a comment that doesn't end
val josuke = 4
```

To fix, close the comment with `*)`.

```sml
(* ok *)
val kujo = 3
(* a comment that ends *)
val josuke = 4
```

Note that comments may be nested.

## 2003

A type variable name was incomplete.

```sml
(* error *)
val xs : ' list = []
```

To be complete, a type variable name must be the following in sequence:

1. One or more ticks: `'`
1. An alphabetic character: `A-Z` or `a-z`
1. Zero or more alphanumeric characters or ticks: `A-Z`, `a-z`, `0-9`, or `'`

To fix, use a valid type variable name.

```sml
(* ok *)
val xs : 'a list = []
```

## 2004

A `string` literal was not closed. String literals start and end with `"`.

```sml
(* error *)
val greeting = "hello there
```

To fix, close the string literal with `"`.

```sml
(* ok *)
val greeting = "hello there"
```

This error may occur when trying to embed `"` in a string literal. To embed `"` in a string literal, use `\"`.

```sml
(* ok *)
val greeting = "he jumped down and said \"hello there\" to the general."
```

## 2005

A `word` literal was negative. Words cannot be negative.

```sml
(* error *)
val neg = ~0w123
```

To fix, use a different type, like `int`, or remove the negative sign.

## 2006

A `char` literal contained more (or less) than 1 character.

```sml
(* error *)
val tooBig = #"hello there"
val tooSmall = #""
```

To fix, make the character literal 1 character long, or use a string literal.

```sml
(* ok *)
val justRight = #"h"
val greeting = "hello there"
val empty = ""
```

## 2007

A number (`int`, `word`, or `real`) literal was incomplete. For instance, a word literal that starts with `0w`, but then with no digits following, is incomplete. Or a real literal that has no digits after the decimal point, marked with `.`, or exponent, marked with `e` or `E`.

```sml
(* error *)
val x : word = 0w
val y : real = 1.
val z : real = 1e
```

To fix, add some digits.

```sml
(* ok *)
val x : word = 0w123
val y : real = 1.123
val z : real = 1e123
```

## 2008

A string escape was invalid.

```sml
(* error *)
val _ = "this doesn't work: \c"
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
(* ok *)
val _ = "this has\na newline"
```

## 2009

There was a non-whitespace character in a string continuation.

```sml
(* error *)
val _ =
  "this string is\  not
  \ valid because there\ are
  \ non-whitespace\ characters
  \ in the continuations"
```

String literals permit the sequence `\...\`, where `...` represents 1 or more whitespace characters. The sequence is ignored. We dub such sequences "string continuations", since they are often used to "continue" strings across lines.

To fix, ensure the string continuations contain only whitespace. Millet recognizes all characters defined in the Definition as whitespace, as well as some others, like carriage return (common on Windows).

```sml
(* ok *)
val _ =
  "this string is\
  \ valid because there are only\
  \ whitespace characters\
  \ in the continuations"
```

## 3001

A name that was not declared infix was used as infix.

```sml
(* error *)
datatype t = C of int * int
fun add (a C b) = a + b
```

To fix, use the name as non-infix, or declare the name as infix.

```sml
(* ok *)
datatype t = C of int * int
fun add (C (a, b)) = a + b
infix C
fun mul (a C b) = a * b
```

## 3002

A name that was declared infix was used as non-infix without the required preceding `op` keyword.

```sml
(* error *)
val _ = + (2, 3)
```

To fix, use the name infix, or add `op`.

```sml
(* ok *)
val _ = 2 + 3
val _ = op+ (2, 3)
```

## 3003

A fixity declaration was invalid.

This can happen when the fixity is too large.

```sml
(* error *)
infix 123456789123456789 foo
```

To fix, only use small-ish fixities.

```sml
(* ok *)
infix 9 foo
```

## 3004

A fixity declaration was negative.

```sml
(* error *)
infix ~3 foo
```

To fix, only use non-negative fixities. Zero is allowed. In fact, zero is implied when a fixity number is not given.

```sml
(* ok *)
infix 3 foo
infix 0 bar
infix quz
```

## 3005

Consecutive infix names with the same fixity, but different associativity, were used without parentheses to disambiguate.

```sml
(* error *)
infix <<
infixr >>
fun a << b = a + b
fun a >> b = a * b
val _ = 1 << 2 >> 3
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
(* error *)
structure

val platinum = 3
```

This is the most common kind of parse error. It's not easy to give general advice for how to fix it.

One bit of advice is this: Since the parser tries to continue parsing a file even in the face of errors, it may find further errors after the first one. But these errors may be all ultimately because of that first error. So, try looking at the first error in the file first.

## 3007

There was an unnecessary usage of `op`.

```sml
(* warning *)
exception op E
val op x = 3
```

To fix, remove the `op`.

## 4001

In a `fun` binding with multiple cases, the cases did not all name the same function.

```sml
(* error *)
fun jonathan 1 = 2
  | dio _ = 3
```

To fix, use a consistent name for the function.

```sml
(* ok *)
fun jonathan 1 = 2
  | jonathan _ = 3
```

## 4002

In a `fun` binding with multiple cases, the cases did not all have the same number of patterns.

```sml
(* error *)
fun muska 1 = 2
  | muska _ _ _ = 3
```

To fix, use a consistent number of patterns across all cases.

```sml
(* ok *)
fun muska 1 = 2
  | muska _ = 3
```

## 4003

An integer (`int` or `word`) literal was invalid. This can happen when it is too large.

```sml
(* error *)
val _ = 0w123456789123456789123456789123456789
```

To fix, use smaller literals.

```sml
(* ok *)
val _ = 0w123456789
```

## 4004

A real literal was invalid.

NOTE: It's not known whether this is currently emitted. It may be that the lexer/parser/lowering setup means that this is handled by earlier stages. However, we should probably emit this if a real literal was too large to be accurately represented or something of that ilk.

## 4005

A numeric label (as for a record) was invalid. This can happen when it was non-positive (i.e. negative or zero), or too large.

```sml
(* error *)
val _ = { 123456789123456789123456789123456789 = "hi" }
```

To fix, use small positive numbers for labels.

```sml
(* ok *)
val _ = { 3 = "hi" }
```

## 4006

There were multiple `...` rest pattern rows.

```sml
(* error *)
val {a, ..., ...} = {a = 1, b = "hi"}
```

To fix, only provide one such row.

```sml
(* ok *)
val {a, ...} = {a = 1, b = "hi"}
```

## 4007

There was a non-`...` pattern row after a `...` pattern row.

```sml
(* error *)
val {..., b} = {a = 1, b = "hi"}
```

To fix, put the `...` pattern row last.

```sml
(* ok *)
val {b, ...} = {a = 1, b = "hi"}
```

## 4008

There was a bar (aka `|`) before the first:

- Case in a `fun` declaration.
- Case in a `fn`, `case`, or `handle` expression.
- Constructor in a `datatype` declaration or case.

```sml
(* error *)
datatype d =
| Chihiro
| Sheeta
```

To fix, remove the bar.

```sml
(* ok *)
datatype d =
  Chihiro
| Sheeta
```

## 4009

There was an `open` or `include` without operands.

```sml
(* error *)
open
```

To fix, give them some operands, or delete the empty `open` or `include`.

```sml
(* ok *)
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
(* error *)
val s =
  let
    structure Integer = Int
    val x = 3
  in
    Integer.toString x
  end
```

To fix, move the declaration to an allowed position.

```sml
(* ok *)
structure Integer = Int
val s =
  let
    val x = 3
  in
    Integer.toString x
  end
```

## 4011

`op` does not work with `andalso` or `orelse`.

```sml
(* error *)
fun bigAnd bs = List.foldl (op andalso) true bs
fun bigOr bs = List.foldl (op orelse) false bs
```

`andalso` and `orelse` are SML keywords, and short-circuit. They are not infix identifiers. Because of this, they do not work with `op`.

To fix, use a lambda or helper function.

```sml
(* ok *)
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
(* error *)
fun foo x =
  let
    val y = 3
    if x = 4 then
      y = 5
    else
      ()
  in
    y + x
  end
```

Instead, we can define `y` as the result of evaluating an `if` expression.

```sml
(* ok *)
fun foo x =
  let
    val y =
      if x = 4 then
        5
      else
        3
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

```sml
(* error *)
fun bar x =
  let
    val y = x + 1;
    print (Int.toString y ^ "\n");
  in
    y
  end
```

Here, the solution could either be to:

- Use a val binding in the `let ... in`:

  ```sml
  (* ok *)
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
  (* ok *)
  fun bar x =
    let
      val y = x + 1
    in
      print (Int.toString y ^ "\n");
      y
    end
  ```

## 4013

A `val` specification had an explicit type variable sequence.

```sml
(* error *)
signature MAPPABLE = sig
  type 'a t
  val ('a, 'b) map : ('a -> 'b) -> ('a t -> 'b t)
end
```

Explicit type variables sequences are permitted for `val` declarations, but not `val` specifications.

To fix, remove the type variable sequence.

```sml
(* ok *)
signature MAPPABLE = sig
  type 'a t
  val map : ('a -> 'b) -> ('a t -> 'b t)
end
```

## 4014

There were unnecessary parentheses around something.

```sml
(* warning *)
val _ = (3)
fun inc (x) = x + 1
type t = (int)
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

## 4015

There was an overly complex expression involving `bool`s.

```sml
(* warning *)
fun booleanIdentity x = if x then true else false
```

An expression is "overly complex" if it involves `if`, `andalso`, or `orelse`, and contains a `bool` literal `true` or `false`. Such expressions can always be simplified. For example:

| Complex                     | Simple        |
| --------------------------- | ------------- |
| `if x then true else false` | `x`           |
| `if x then false else true` | `not x`       |
| `if x then y else false`    | `x andalso y` |
| `if x then true else y`     | `x orelse y`  |
| `if true then x else y`     | `x`           |
| `if false then x else y`    | `y`           |
| `x orelse true`             | `true`        |
| `x orelse false`            | `x`           |
| `x andalso true`            | `x`           |
| `x andalso false`           | `false`       |

To fix, simplify the expression.

## 4016

There was a `case` expression with only one arm.

```sml
(* warning *)
datatype d = D of int
fun toInt x = case x of D y => y
```

- An exhaustive case with only one arm is better expressed as an irrefutable binding, e.g. with `val`.
- A non-exhaustive case with only one arm should be made exhaustive.

To fix, rewrite the `case` as something else, or add more arms.

```sml
(* ok *)
datatype d = D of int
fun toInt (D y) = y
```

## 4017

There was an unnecessary semicolon.

```sml
(* warning *)
val x = 3;
val y = "hi";
```

Semicolons are used in a REPL setting to indicate the end of input, but are unnecessary in most cases in source files.

To fix, remove the semicolon.

## 4999

There was an occurrence of an unsupported SML construct.

```sml
(* error *)
val x = #[1, 2]
```

At time of writing, the following constructs are not supported:

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
(* error *)
val _ = foo
```

To fix, try any of the following:

- Check that the name is correctly spelled.
- Check the if the name is defined in the current scope unqualified, or if it is in a structure. For instance, `filter` is defined in `structure List`, not at the top level. Some functions like `map` are defined both in `List` and at the top level.
- Check the error message to see what kind of thing was not defined: value, type, structure, etc. These different kinds of items have different namespaces.

  In this example, there is a value named `x` defined, but then we try to use `x` as a type. There is no type `x` defined, so this is invalid.

  ```sml
  (* error *)
  val x = 4
  val y : x = 5
  ```

- Check that the name is not explicitly forbidden from being accessed by a signature ascription. Ascribing a structure to a more restrictive signature prohibits accessing the "extra" items inside the structure.

  ```sml
  (* error *)

  signature SIG = sig
    val foo : int
  end

  structure Str : SIG =
    val foo = 3
    val bar = "hi"
  end

  val _ = Str.bar
  ```

## 5002

There was a duplicate of something.

This may occur when using `and` to declare many things at once.

```sml
(* error *)
val x = 3
and x = 4
```

It may also occur when binding the same name more than once in a pattern.

```sml
(* error *)
fun add (x, x) = x + x
```

To fix, use different names, or avoid `and`. (The latter induces shadowing.)

```sml
(* ok *)
val x = 3
val x = 4
```

## 5003

Something was requested by a signature, but not present in the structure that is attempting to ascribe to that signature.

```sml
(* error *)
signature SIG = sig
  val x : int
end

structure Str : SIG = struct end
```

To fix, provide definitions for the missing items.

One type of situation in which this error may confusingly appear involves "syntax sugar" around functor definition and application. Here's an example:

```sml
(* error *)
signature SIG = sig type t end
structure Arg : SIG = struct type t = unit end
functor Func (structure Param : SIG) = struct ... end
structure S = Func (Arg)
```

This is invalid, but the reason why is subtle. Before we explain the reason, note that the bottom line is that the call site of `Func` must be changed:

```diff
-structure S = Func (Arg)
+structure S = Func (structure Param = Arg)
```

The fact that both the definition site and the corrected call site for `Func` have an extra `structure` keyword is a clue.

The key is in the definition of `Func`. We use the syntax:

<!-- @ignore -->

```sml
functor Func (structure Param : SIG)
```

which has a distinct meaning from:

<!-- @ignore -->

```sml
functor Func (Param : SIG)
```

Both forms are legal SML. The first form is syntax sugar. This means it is extra "helper" syntax that is fully defined in terms of the second, more "fundamental" syntax.

As an aside, another example of syntax sugar is how list literals like `[2, 4, 6]` "desugar" to usages of the list constructors `::` and `nil`, namely `2 :: 4 :: 6 :: nil`.

In the example, the sugar is expanded in the following manner:

<!-- @ignore -->

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

<!-- @ignore -->

```sml
(* original *)
structure S = Func (structure Param = Arg)

(* desugared *)
structure S = Func (struct structure Param = Arg end)
```

A similar but "opposite" error may occur if the definition site does not use the syntax sugar, but the call site does. As in:

<!-- @ignore -->

```sml
functor Func (Param : SIG) = struct ... end
structure S = Func (structure Param = Arg)
```

This will error. To fix, do not use the syntax sugar at the call site.

```diff
-structure S = Func (structure Param = Arg)
+structure S = Func (Arg)
```

## 5004

Something was not requested by a signature, but was present in the structure that is attempting to ascribe to that signature.

Usually, this is allowed, but it is forbidden for `datatype` declarations.

```sml
(* error *)
signature SIG = sig
  datatype d = Pazu
end

structure Str : SIG = struct
  datatype d = Pazu | Sosuke
end
```

To fix, ensure only the requested items are defined.

## 5005

Typechecking failed, because of "circularity", which means we attempted to a set a type variable to be equal to a type containing that type variable itself.

Consider this example:

```sml
(* error *)
fun f x = x x
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

Two types that were supposed to be "the same" were not.

This is probably the most common typechecking error, so it's hard to give general advice for how to fix it.

Millet tries to report which type was "expected" and which was "found". For instance, in this example, we consider `int` the "expected" type, because of the annotation. This explicit annotation implies the programmer really thought it should be that type.

```sml
(* error *)
val x : int = "no"
```

This hints at a possible strategy for debugging this kind of error: if the expected and found types are confusing, try adding more type annotations.

This error commonly occurs when applying a function to an argument, but the argument did not have the type the function expected. For instance, in this example, Millet reports that we "expected" `bool`, because the function `choose` takes a `bool`.

```sml
(* error *)
fun choose x = if x then "sosuke" else "pazu"
val _ = choose 4
```

Note that certain built-in functions, like `+`, `<`, and `abs` are overloaded, which means they may work with a certain fixed number of types. For instance, `+` works with `int`, `word`, and `real`, while `<` works for those as well as `string` and `char`.

Millet reports these overloaded types with intentionally invalid SML syntax. Here is what they mean:

| Overload    | Meaning                                 |
| ----------- | --------------------------------------- |
| `<wordint>` | `word`, `int`                           |
| `<realint>` | `real`, `int`                           |
| `<num>`     | `word`, `real`, `int`                   |
| `<numtxt>`  | `word`, `real`, `int`, `string`, `char` |

When using overloaded functions, there must exist a single actual type being used. For instance, `+` is overloaded as `<num>`, which means it works with `word`, `real`, and `int`. However, `+` cannot add a `real` to a `word`, or an `int` to a `real`, or any such similar combination. It can only add two `word`s, or two `real`s, or two `int`s.

Further, Millet will report type variables that haven't been "solved" yet with the syntax `?a`, `?b`, etc, which is, again, intentionally invalid SML syntax.

Finally, if Millet encounters an invalid expression, like a variable that was undefined, it will report the type `_`. Although Millet is able to parse `_` as a type "hole" if written in code, this again is not valid SML syntax.

## 5007

A function application expression was encountered, but the function expression did not have a function type.

```sml
(* error *)
val _ = "foo" 3
```

In this example, we attempt to treat the string `"foo"` as a function and apply it to the argument `3`.

This error is a special case of 5006, specialized for the common case of function application.

To fix, only apply functions to arguments.

## 5008

There was a duplicate label.

```sml
(* error *)
val _ = { a = 1, a = 2 }
```

To fix, use differently named labels, or remove one of the record rows.

## 5009

A real literal was used as a pattern.

```sml
(* error *)
fun f (x : real) : int =
  case x of
    1.2 => 3
  | 1.4 => 5
  | _ => 6
```

To fix, consider checking that the given real is within some epsilon value of the desired real.

Usage of `Real.==` to check for equality between reals is discouraged, due to limitations around representing floating-point (aka, real) numbers on most architectures.

## 5010

A pattern in a `case` expression or similar (like `handle`) was not reachable.

Patterns in a `case` are tried from top to bottom. If a pattern further up the `case` always matches certain values, then the lower pattern will never be reached. Thus, the lower pattern is unreachable.

```sml
(* error *)
fun f x =
  case x of
    1 => 2
  | 1 => 3
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
(* error *)
datatype d = A of string | B of int

fun f (x : d) : int =
  case x of
    B y => y
```

To fix, add patterns matching the missing cases. The error message reports examples of patterns not matched.

## 5012

This is effectively the same error as 5011, but it emitted for singular bindings, like with `val`.

```sml
(* error *)
datatype d = A of string | B of int

fun f (x : d) : string =
  let
    val A y = x
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
(* error *)
structure S = struct
  val x = 3
end

fun f y =
  case y of
    S.x => 1
  | 4 => 5
  | _ => 6
```

To fix, use a literal pattern, or check for equality another way, for instance with `=`.

```sml
(* ok *)
fun f y =
  case y of
    3 => 1
  | 4 => 5
  | _ => 6
```

## 5014

A constructor had an argument in a pattern match, but it was defined to have no argument.

```sml
(* error *)
datatype d = A | B of int

fun f x =
  case x of
    A y => y + 1
  | B z => z - 1
```

To fix, define the constructor to have an argument, or remove the argument from the pattern.

## 5015

A constructor had no argument in a pattern match, but it was defined to have an argument.

```sml
(* error *)
datatype d = A | B of int

fun f x =
  case x of
    A => 1
  | B => 2
```

To fix, define the constructor to not have an argument, or add an argument to the pattern.

## 5016

An invalid name was used as the left hand side of an `as` pattern.

As-patterns allow binding the entirety of a pattern `p` to a name `n` with the pattern `n as p`. However, the name `n` may not, for instance, already exist as a non-value:

```sml
(* error *)
exception Bad
fun f x =
  case x of
    Bad as _ => 1
  | _ => 2
```

To fix, use a valid name.

## 5017

A type name escapes the scope in which it is valid.

Here, the type `d` is only available for the scope of the `let` expression, but the `let` expression would cause a value of type `d` to "escape" the `let` and be bound to `x`, outside the `let`.

```sml
(* error *)
val x =
  let
    datatype d = D
  in
    D
  end
```

To fix, extend the scope of the type, or do not allow its values to escape its scope.

## 5018

In a `val rec` binding, the expression must be a literal `fn` expression.

```sml
(* error *)
val rec x = x + 3
```

It is an error even if the expression does not use the recursive binding.

```sml
(* error *)
val rec x = 3
```

It is also an error even if the expression has function type.

```sml
(* error *)
val mkAdd3 = fn () => fn x => x + 3
val rec add3 = mkAdd3 ()
```

To fix, ensure the expression is a literal `fn` expression.

## 5019

The wrong number of type arguments was passed to a type-level function.

```sml
(* error *)
type ('a, 'b) pair = 'a * 'b
type nope = int pair
```

`datatype`s, like `'a list`, also define type-level functions.

```sml
(* error *)
val xs : list = []
```

To fix, pass the correct number of type arguments.

```sml
(* ok *)
type ('a, 'b) pair = 'a * 'b
type yep = (int, string) pair
val xs : yep list = []
```

## 5020

In an exception copy declaration, the right hand side was not an exception.

```sml
(* error *)
val x = 3
exception Nope = x
```

To fix, only use exceptions on the right hand side.

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
(* error *)
val false = 123
```

To fix, do not attempt to rebind these names.

## 5022

Names have "statuses", which can be one of:

- exception
- constructor
- value

These statuses must be compatible for the purposes of matching a structure against a signature.

```sml
(* error *)
exception Foo

structure S : sig
  exception E
end = struct
  val E = Foo
end
```

To fix, ensure the names have compatible statuses.

```sml
(* ok *)
exception Foo

structure S : sig
  exception E
end = struct
  exception E = Foo
end
```

## 5023

A record type couldn't be fully resolved, due to the use of a `...` pattern row.

```sml
(* error *)
fun getX {x, ...} = x
```

SML lacks row polymorphism, so the above example function does not typecheck.

To fix, consider adding a type annotation.

```sml
(* ok *)
type t = {x : int, y : bool, z : string}
fun getX ({x, ...} : t) = x
```

This error may arise when using `#` selectors.

```sml
(* error *)
fun addFooBar x = #foo x + #bar x
```

Again, the fix is usually to add a type annotation. Though, an alternative would be to avoid `...` pattern rows altogether.

```sml
(* ok *)
fun addFooBar {foo, bar} = foo + bar
```

## 5024

Not all or pattern alternatives bound the same names.

```sml
(* error *)
datatype t = Foo of int | Bar of int

fun toInt (x : t) : int =
  case x of
    (Foo y | Bar _) => y
```

To fix, ensure all alternatives bind the same names. The types must also match.

```sml
(* ok *)
datatype t = Foo of int | Bar of int

fun toInt (x : t) : int =
  case x of
    (Foo y | Bar y) => y
```

Note that or patterns are not permitted by the Definition, though they are a common extension, implemented by SML/NJ and MLton.

## 5025

A `signature` or `functor` declaration occurred in a disallowed position, like inside `struct ... end`.

```sml
(* error *)
structure Str = struct
  signature SIG = sig end
  functor Func() = struct end
end
```

To fix, declare the signature or functor at the top level.

```sml
(* ok *)
structure Str = struct end
signature SIG = sig end
functor Func() = struct end
```

Although not permitted by the Definition, Millet also allows defining the signature or functor in a `local`.

```sml
(* ok *)
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
(* error *)
val answer = if _ then "yes" else "no"
```

The error message contains information about the type of the hole given the surrounding context. For instance, in the above example, the hole is reported to have type `bool` because it is being used as the condition to an `if` expression.

Expression holes can either be `_` or `...`.

```sml
(* error *)
fun f x = ...
val _ = f 5
```

To fix, replace the hole with a real expression of the correct type.

## 5027

There was a type hole.

```sml
(* error *)
type thing = string * _ list * int
```

Type holes can be either `_` or `...`.

```sml
(* error *)
type func = ... -> ...
```

To fix, replace the hole with a real type.

## 5028

There was a declaration hole.

```sml
(* error *)
structure S = struct
  ...
end
```

Declaration holes are written `...`.

Sometimes `...` is used in declaration position as a placeholder or filler in examples, for example in the discussion of functor argument interface syntax sugar in the description of 5003.

To fix, replace or remove the hole.

## 5029

An attempt was made to bind an expansive expression to a variable, where that variable would then have polymorphic type.

This error is also called the "value restriction".

An expansive expression is one that may raise an exception or allocate memory during evaluation. Some examples of expansive expressions:

| Name                   | Example                       |
| ---------------------- | ----------------------------- |
| Application (see note) | `f x`                         |
| Raise                  | `raise Bad`                   |
| Handle                 | `e handle Bad => e'`          |
| Let                    | `let val x = e in x + e' end` |

Note that:

- An application expression is not considered expansive if the function is a constructor, e.g. `SOME`, and the argument is non-expansive.
- However, any expression containing the `ref` constructor is expansive.

A polymorphic type is one which contains type variables, like `'a` or `'b`.

If Millet did not emit an error for cases like this, we could break type safety:

```sml
(* error *)
val r : 'a option ref = ref NONE
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
  (* error *)
  val mapFst =
    List.map (fn (x, _) => x)
  ```

  After:

  ```sml
  (* ok *)
  fun mapFst xs =
    List.map (fn (x, _) => x) xs
  ```

- Annotate the expression (or pattern) with a non-polymorphic type.

  Before:

  ```sml
  (* error *)
  val r = ref []
  ```

  After:

  ```sml
  (* ok *)
  val r : int list ref = ref []
  ```

## 5030

The left-hand side of an `as` pattern was neither a name nor a typed name.

```sml
(* error *)
fun f x =
  case x of
    3 as y => 8
  | _ => x
```

The syntax for `as` patterns is `<name> (: <ty>)? as <pat>`.

To fix, ensure the left hand side of the `as` is either a name or a typed name.

```sml
(* ok *)
fun f x =
  case x of
    y as 3 => 8
  | _ => x
```

## 5031

There was an unused variable.

```sml
(* warning *)
fun ignoreArg x = 3
```

To fix, use the variable, or do not define it.

```sml
(* ok *)
fun useArg x = x + 3
fun doNotBindArg _ = 3
```

## 5999

There was an occurrence of an unsupported SML construct.

```sml
(* error *)
abstype t = T with val _ = 3 end
```

At time of writing, the following constructs are not supported:

- `abstype` declarations.

[config]: /docs/config.md
