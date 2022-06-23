# Error reference

This is (or rather, should be) a list of all the errors that Millet can emit, and what they mean. If Millet emitted an error not detailed here, that's a bug, and we would appreciate if you file an issue.

## 1001

There was an invalid character in the source file.

```sml
(* error *)
val 空条承太郎 = 3
```

Millet only allows certain ASCII characters to appear in names and the like. String literals and comments, however, should be able to handle arbitrary UTF-8.

To fix, only use allowed source characters. Only ASCII characters (but not all ASCII characters) are allowed.

```sml
(* ok *)
val kujoJotaro = 3
```

## 1002

There was an unclosed comment. This means an open comment delimiter `(*` was not matched by a later close comment delimiter `*)`.

```sml
(* error *)
val x = 3
(* a comment that doesn't end
val y = 4
```

To fix, close the comment with `*)`.

```sml
(* ok *)
val x = 3
(* a comment that ends *)
val y = 4
```

Note that comments may be nested.

## 1003

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

## 1004

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
val greeting = "he jumped down and said \"hello there\" aloud."
```

## 1005

A `word` literal was negative. Words cannot be negative.

```sml
(* error *)
val neg = ~0w123
```

## 1006

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

## 1007

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

## 1008

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

## 1009

There was a non-whitespace character in a string continuation.

```sml
(* error *)
val _ =
  "this string is\  not
  \ valid because there\ are
  \ non-whitespace\ characters
  \ in the continuations."
```

String literals permit the sequence `\...\`, where `...` represents 1 or more whitespace characters. Whitespace is space, tab, newline, and vertical tab. The sequence is ignored. We dub such sequences "string continuations", since they are often used to "continue" strings across lines.

```sml
(* ok *)
val _ =
  "this string is\
  \ valid because there are only\
  \ whitespace characters\
  \ in the continuations."
```

## 2001

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

## 2002

A name that was declared infix was used as non-infix without the required preceding `op` keyword.

```sml
(* error *)
val _ = + (2, 3)
```

To fix, use the name infix, or add `op`.

```sml
(* ok *)
val _ = 2 + 3
val _ = op + (2, 3)
```

## 2003

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

## 2004

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

## 2005

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

To fix:

- Add parentheses to disambiguate.
- Or use different fixities.
- Or use the same associativity.
- Or use `op`.

## 2006

The parser expected something, but it didn't find it. For example, in this case, the parser expected a `=` after the name `S`.

```sml
(* error *)
structure S struct end
```

This is the most common kind of parse error. It's not easy to give general advice for how to fix it.

One bit of advice is this: Since the parser tries to continue parsing a file even in the face of errors, it may find further errors after the first one. But these errors may be all ultimately because of that first error. So, try looking at the first error in the file first.

## 3001

There was an occurrence of an unsupported SML construct.

```sml
(* error *)
val x = #[1, 2]
```

At time of writing, the following constructs are not supported:

- Vector expressions.
- Vector patterns.
- Or patterns.

Note that these constructs are not defined by the Definition, but are somewhat common extensions in implementations like SML/NJ (by default) and MLton (optionally).

To fix, avoid such constructs.

## 3002

In a `fun` binding with multiple cases, the cases did not all name the same function.

```sml
(* error *)
fun f 1 = 2
  | g _ = 3
```

To fix, use a consistent name for the function.

```sml
(* ok *)
fun f 1 = 2
  | f _ = 3
```

## 3003

In a `fun` binding with multiple cases, the cases did not all have the same number of patterns.

```sml
(* error *)
fun f 1 = 2
  | f _ _ = 3
```

To fix, use a consistent number of patterns across all cases.

```sml
(* ok *)
fun f 1 = 2
  | f _ = 3
```

## 3004

An integer literal was invalid. This can happen when it is too large.

```sml
(* error *)
val _ = 123456789123456789123456789123456789
```

To fix, use smaller literals.

```sml
(* ok *)
val _ = 123456789
```

## 3005

A real literal was invalid.

NOTE: It's not known whether this is currently emitted. It may be that the lexer/parser/lowering setup means that this is handled by earlier stages. However, we should probably emit this if a real literal was too large to be accurately represented or something of that ilk.

## 3006

A numeric label (as for a record) was invalid. This can happen when it was negative, or too large.

```sml
(* error *)
val _ = { 123456789123456789123456789123456789 = "hi" }
```

To fix, use small positive numbers for labels.

```sml
(* ok *)
val _ = { 3 = "hi" }
```

## 3007

A numeric label (as for a record) was zero. Numeric labels must not be zero.

```sml
(* error *)
val _ = { 0 = "hi" }
```

To fix, do not use zero as a numeric label.

## 3008

A `signature` or `functor` declaration occurred not at the top level.

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

## 3009

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

## 3010

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

## 4001

There was an occurrence of an unsupported SML construct.

```sml
(* error *)
abstype t = T with val _ = 3 end
```

At time of writing, the following constructs are not supported:

- `abstype` declarations.

## 4002

A name was referenced, but it was not defined in that scope.

```sml
(* error *)
val _ = foo
```

To fix, try any of the following.

(1) Check that the name is correctly spelled.

(2) Check the if the name is defined in the current scope unqualified, or if it is in a structure. For instance, `filter` is defined in `structure List`, not at the top level. Some functions like `map` are defined both in `List` and at the top level.

(3) Check the error message to see what kind of thing was not defined: value, type, structure, etc. These different kinds of items have different namespaces.

In this example, there is a value named `x` defined, but then we try to use `x` as a type. There is no type `x` defined, so this is invalid.

```sml
(* error *)
val x = 4
val y : x = 5
```

(4) Check that the name is not explicitly forbidden from being accessed by a signature ascription. Ascribing a structure to a more restrictive signature prohibits accessing the "extra" items inside the structure.

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

## 4003

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

## 4004

Something was requested by a signature, but not present in the structure that is attempting to ascribe to that signature.

```sml
(* error *)
signature SIG = sig
  val x : int
end

structure Str : SIG = struct end
```

To fix, provide definitions for the missing items.

## 4005

Something was not requested by a signature, but was present in the structure that is attempting to ascribe to that signature.

Usually, this is allowed, but it is forbidden for `datatype` declarations.

```sml
(* error *)
signature SIG = sig
  datatype d = A
end

structure Str : SIG = struct
  datatype d = A | B
end
```

To fix, ensure only the requested items are defined.

## 4006

Typechecking failed, because of "circularity", which means we attempted to a set a type variable to be equal to a type containing that type variable itself.

"Huh?", you may say. Consider this example:

```sml
(* error *)
fun f x = x x
```

When Millet attempts to typecheck `f`, it first assigns a fresh type variable to the name `x`. Then it enters the body of `f` to see how `x` is used, updating the type variable for `x` as it goes. Let's use `$x` as the type variable for `x`.

We see the application expression `x x`. `x` is used as a function, so it must have a type like `$1 -> $2` where `$1` and `$2` are types. So, we have the constraint `$x = $1 -> $2`.

However, `x` is used as the argument to that function, which we just said has type `$1 -> $2`. So we must have `$x = $1`, the type of the argument to the function.

We now have

- `$x = $1 -> $2`
- `$x = $1`

Substituting, we have `$1 = $1 -> $2`. That is, we are setting a type variable, in this case `$1`, to a type, in this case `$1 -> $2`, that contains itself. This is not allowed.

## 4007

Two types that were supposed to be "the same" were not.

This is probably the most common typechecking error, so it's hard to give general advice for how to fix it.

Millet tries to report which type was "expected" and which was "found". For instance, in this example, we consider `int` the "expected" type, because of the annotation. This explicit annotation implies the programmer really thought it should be that type.

```sml
(* error *)
val x : int = "no"
```

This error commonly occurs when applying a function to an argument, but the argument did not have the type the function expected. For instance, in this example, Millet reports that we "expected" `string`, because the top-level function `print` function takes a `string`.

```sml
(* error *)
val () = print 3
```

## 4008

This is similar to 4007, but it was with an overloaded function type.

Certain built-in functions, like `+`, `<`, and `abs` are overloaded, which means they may work with a certain fixed number of types. For instance, `+` works with `int`, `word`, and `real`, while `<` works for those as well as `string` and `char`.

## 4009

A function application expression was encountered, but the function expression did not have a function ("arrow") type.

```sml
(* error *)
val _ = "foo" 3
```

This is a special case of 4007, specialized for the common case of function application.

## 4010

There was a duplicate label.

```sml
(* error *)
val _ = { a = 1, a = 2 }
```

To fix, use differently named labels, or remove one of the record rows.

## 4011

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

## 4012

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

## 4013

A `case` expression (or similar) was not exhaustive.

When we `case` on a value of a given type, we must assume that value can be any possible value of that type. If it is not the case that for every possible value of that type, there exists a pattern in the `case` that matches that value, then the `case` is not exhaustive.

```sml
(* error *)
datatype d = A | B of int

fun f (x : d) : int =
  case x of
    A => 1
```

To fix, add patterns matching the missing cases. The error message reports examples of patterns not matched.

## 4014

This is effectively the same error as 4013, but it emitted for singular bindings, like with `val`.

```sml
(* error *)
datatype d = A | B of int

fun f (x : d) : int =
  let
    val B y = x
  in
    y
  end
```

The pattern in a `val` binding ought to be "irrefutable", to wit, it alone ought to match all possible values of the type of the expression. For example:

- The wildcard pattern `_` is irrefutable.
- Variable patterns like `x` and `y` are irrefutable.
- Tuple patterns composed of irrefutable patterns are irrefutable.
- More generally, if a `datatype` has only one constructor, a pattern with that one constructor, and an irrefutable pattern argument (if one is needed) is irrefutable.

## 4015

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

## 4016

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

## 4017

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

## 4018

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

## 4019

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

## 4020

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
val mkFn = fn () => fn x => x + 1
val rec inc = mkFn ()
```

To fix, ensure the expression is a literal `fn` expression.

## 4021

The wrong number of type arguments was passed to a type-level function.

```sml
(* error *)
type ('a, 'b) pair = 'a * 'b
type nope = int pair
```

`datatype`s, like `'a list`, also define type-level functions.

```sml
(* error *)
val xs: list = []
```

To fix, pass the correct number of type arguments.

## 4022

In an exception copy declaration, the right hand side was not an exception.

```sml
(* error *)
val x = 3
exception Nope = x
```

To fix, only use exceptions on the right hand side.

## 4023

Certain names in certain namespaces may not be rebound. These names are:

| Name    | Definition                                         |
| ------- | -------------------------------------------------- |
| `true`  | logical truth                                      |
| `false` | logical falsity                                    |
| `nil`   | the empty list constructor                         |
| `::`    | the non-empty list constructor                     |
| `ref`   | the reference type constructor                     |
| `=`     | the polymorphic equality function                  |
| `it`    | the value of the last expression entered in a REPL |

```sml
(* error *)
val false = 123
```

To fix, do not attempt to rebind these names.

## 4024

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

## 4025

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

## 4026

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
