# Primitives

This is documentation for various SML primitive functions and types.

## `bool`

The built-in boolean type.

There are two boolean values: `true` and `false`.

```sml
val yes = true
val no = false
```

Functions that check for equality or perform comparisons, like `=` and `>`, often return `bool`.

```sml
val yep = "foo" ^ "bar" = "foobar"
val nope = 3 > 7
```

`if` expressions case on the value of a `bool`.

```sml
fun choose (b : bool) = if b then "yea" else "nah"
```

The `Bool` structure provides operations on booleans.

```sml
val s = Bool.toString false
```

## `char`

The built-in character type.

Literals are written like string literals (delimited with `"`), except they must:

- Have length 1.
- Have a leading `#`.

```sml
val percent = #"%"
val space = #" "
```

Millet, and SML in general, has limited support for extended character sets beyond ASCII.

The `Char` structure provides operations on characters.

```sml
val no = Char.isSpace #"g"
```

## `int`

The built-in integer type.

Integers may be positive, negative, or 0. They are usually written with decimal digits.

```sml
val pos = 4
val neg = ~8
val zero = 0
```

They can also be written in hex, with the prefix `0x`.

```sml
val hex = 0x123beef
```

The `Int` structure provides operations on integers.

```sml
val three = Int.min (3, 5)
```

## `real`

The built-in real number type.

This type approximates the "mathematical" real numbers by using [floating-point arithmetic][fp].

```sml
val r = 1.4
val closeToZeroPoint3 = 0.1 + 0.2
```

Literals can have an exponent with an `e` or `E` following the base.

```sml
val withExp = 1.4e5
```

The `Real` structure provides operations on reals.

```sml
val four = Real.sqrt 16.0
```

[fp]: https://0.30000000000000004.com

## `ref`

The built-in reference type.

This is the core primitive for imperative programming in SML. It is one of the few types in SML whose values are mutable.

- To construct a ref, use `ref`.
- To get the current value in a ref, use `!`.
- To set the value in a ref to a new value, use `:=`.

```sml
val r = ref 0
val zero = !r
val () = r := 3
val three = !r
```

The `Ref` structure provides operations on references.

```sml
val r1 = ref 3
val r2 = ref 8
val () = Ref.swap (r1, r2)
val eight = !r1
val three = !r2
```

## `string`

The built-in string type.

Literals are delimited with double quotes, `"`.

```sml
val message = "hello, world!"
```

There are various string escapes available, all of which start with `\`.

```sml
val () = print "hi\n"
```

Strings can be concatenated with `^`.

```sml
val foobar = "foo" ^ "bar"
```

The `String` structure provides operations on strings.

```sml
val yes = String.isSubstring "erica" "america"
```

Strings are often thought of as ordered sequences of characters. Indeed, in SML, there is `String.explode` and `String.implode` to go from strings to list of characters and vice versa. However, the notion of "character" is [difficult to define][unicode].

[unicode]: https://home.unicode.org

## `use`

`use` is available in some SML implementations, like [SML/NJ][smlnj], but not others, like [MLton][].

When it is available, usually, the approximate semantics of calling `use f2` from a file `f1` is to "load" the contents of `f2`, and bring everything defined by that file `f2` into the scope of `f1`.

A small example:

```sml
(* f1.sml *)
val x = 3
(* f2.sml *)
use "f1.sml";
val y = x + 4
```

`use` is sometimes used for small-scale multi-file SML projects. For larger projects, consider:

- [SML/NJ Compilation Manager][cm]
- [ML Basis][mlb]

[smlnj]: https://www.smlnj.org
[mlton]: http://mlton.org
[cm]: https://www.smlnj.org/doc/CM/new.pdf
[mlb]: http://mlton.org/MLBasis

## `word`

The built-in word type.

Words are unsigned integers. They are written in decimal digits with the prefix `0w`.

```sml
val dec = 0w123
```

They can also be written in hex with the prefix `0wx`.

```sml
val hex = 0wx123beef
```

The `Word` structure provides operations on words.

```sml
val nine = Word.max (0w4, 0w9)
```
