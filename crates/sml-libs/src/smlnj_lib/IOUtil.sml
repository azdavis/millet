signature IO_UTIL = sig
  val withInputFile : string * ('a -> 'b) -> 'a -> 'b
  val withInstream : TextIO.instream * ('a -> 'b) -> 'a -> 'b
  val withOutputFile : string * ('a -> 'b) -> 'a -> 'b
  val withOutstream : TextIO.outstream * ('a -> 'b) -> 'a -> 'b
end

structure IOUtil : IO_UTIL = struct end
