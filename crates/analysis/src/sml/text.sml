(*!
The TEXT signature collects together various text related structures based on the representation of
the shared character type.

The TEXT signature is matched by two structures, the required Text and the optional WideText. The
former implements strings based on the extended ASCII 8-bit characters. The latter provides strings
of characters of some size greater than or equal to 8 bits.
!*)
signature TEXT = sig
  structure Char : CHAR
  structure String : STRING
  structure Substring : SUBSTRING
  structure CharVector : MONO_VECTOR
  structure CharArray : MONO_ARRAY
  structure CharVectorSlice : MONO_VECTOR_SLICE
  structure CharArraySlice : MONO_ARRAY_SLICE
    sharing type Char.char = String.char = Substring.char = CharVector.elem = CharArray.elem = CharVectorSlice.elem = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string = CharVector.vector = CharArray.vector = CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharVectorSlice.slice = CharArraySlice.vector_slice
end

structure Text :> TEXT
  where type Char.char = Char.char
  where type String.string = String.string
  where type Substring.substring = Substring.substring
  where type CharVector.vector = CharVector.vector
  where type CharArray.array = CharArray.array
  where type CharVectorSlice.slice = CharVectorSlice.slice
  where type CharArraySlice.slice = CharArraySlice.slice = struct end
structure WideText :> TEXT (* OPTIONAL *)
  where type Char.char = WideChar.char
  where type String.string = WideString.string
  where type Substring.substring = WideSubstring.substring
  where type CharVector.vector = WideCharVector.vector
  where type CharArray.array = WideCharArray.array
  where type CharVectorSlice.slice = WideCharVectorSlice.slice
  where type CharArraySlice.slice = WideCharArraySlice.slice = struct end
