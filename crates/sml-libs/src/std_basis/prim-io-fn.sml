(*!
 * The optional functor PrimIO builds an instance of the primitive I/O signature PRIM_IO.
 *)
functor PrimIO (
  structure Vector : MONO_VECTOR
  structure VectorSlice : MONO_VECTOR_SLICE
  structure Array : MONO_ARRAY
  structure ArraySlice : MONO_ARRAY_SLICE
  sharing type Vector.elem = VectorSlice.elem = Array.elem = ArraySlice.elem
  sharing type Vector.vector = VectorSlice.vector = Array.vector = ArraySlice.vector
  sharing type VectorSlice.slice = ArraySlice.vector_slice
  sharing type Array.array = ArraySlice.array
  val someElem : Vector.elem
  eqtype pos
  val compare : pos * pos -> order
) : PRIM_IO = struct end
