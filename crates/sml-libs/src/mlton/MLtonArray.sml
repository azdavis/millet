signature MLTON_ARRAY = sig
  (*!
   * unfoldi (n, b, f) constructs an array a of length n, whose elements ai are determined by the
   * equations b0 = b and (ai, bi+1) = f (i, bi).
   *)
  val unfoldi : int * 'b * (int * 'b -> 'a * 'b) -> 'a array * 'b
end
