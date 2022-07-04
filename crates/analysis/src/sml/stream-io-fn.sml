(*!
The optional StreamIO functor provides a way to build a Stream I/O layer on top of an arbitrary
Primitive I/O implementation. For example, given an implementation of readers and writers for pairs
of integers, one can define streams of pairs of integers.
!*)
functor StreamIO (
  structure PrimIO : PRIM_IO
  structure Vector : MONO_VECTOR
  structure Array : MONO_ARRAY
  sharing type PrimIO.elem = Vector.elem = Array.elem
  sharing type PrimIO.vector = Vector.vector = Array.vector
  sharing type PrimIO.array = Array.array
  val someElem : PrimIO.elem
) : STREAM_IO = struct end
