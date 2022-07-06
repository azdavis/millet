(*!
The optional ImperativeIO functor can be used to implement (derive) an imperative-style stream I/O
facility in terms of a lazy functional stream I/O facility. In the imperative style, input and
output operations do not return a new stream each time but cause side-effects on their arguments.
Most functions can raise the Io exception for various reasons, including illegal or inconsistent
parameters, IO failures, and attempts to do I/O on closed output streams.

The ImperativeIO functor is not often needed, as the required BinIO and TextIO structures supply
imperative-style I/O for most situations. It plays a useful role when the programmer needs to
construct I/O facilities with element types other than char or Word8.word, or ones based on
user-specified I/O primitives.
!*)
functor ImperativeIO (
  structure StreamIO : STREAM_IO
  structure Vector : MONO_VECTOR
  structure Array : MONO_ARRAY
  sharing type StreamIO.elem = Vector.elem = Array.elem
  sharing type StreamIO.vector = Vector.vector = Array.vector
) : IMPERATIVE_IO = struct end
