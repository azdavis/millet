(*!
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
!*)
type int = int

(*!
The built-in word type.

Words are unsigned integers. They are written in decimal digits with the prefix `0w`.

```sml
val dec = 0w123
```

They can also be written in hex with the prefix `0wx`.

```sml
val hex = 0wx123beef
```

The `Word` structure provides operations on integers.

```sml
val nine = Word.max (0w4, 0w9)
```
!*)
type word = word

(*!
The built-in real number type.

This type approximates the "mathematical" real numbers by using [floating-point arithmetic][1].

```sml
val r = 1.4
val closeToZeroPoint3 = 0.1 + 0.2
```

Literals can have an exponent with an `e` or `E` following the base.

```sml
val withExp = 1.4e5
```

The `Real` structure provides operations on integers.

```sml
val four = Real.sqrt 16.0
```

[1]: https://0.30000000000000004.com
!*)
type real = real

(*!
The built-in character type.

Characters are string literals with length 1 and a leading `#`.

```sml
val percent = #"%"
val space = #" "
```

Millet, and SML in general, has limited support for extended character sets beyond ASCII.

The `Char` structure provides operations on characters.

```sml
val no = Char.isSpace #"g"
```
!*)
type char = char

(*!
The built-in string type.

Strings are delimited with double quotes, `"`.

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

Strings are often thought of as ordered sequences of characters. Indeed, in SML, there is
`String.explode` and `String.implode` to go from strings to list of characters and vice versa.
However, the notion of "character" is [difficult to define][1].

[1]: https://home.unicode.org
!*)
type string = string

(*!
The built-in boolean type.

There are two boolean values: `true` and `false`.

```sml
val yes = true
val no = false
```

Functions that check for equality or perform comparisions, like `=` and `>`, often return `bool`.

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
!*)
type bool = bool

(*!
The built-in reference type.

This is the core primitive for imperative programming in SML. It is one of the few types in SML
whose values are mutable.

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
!*)
type 'a ref = 'a ref
