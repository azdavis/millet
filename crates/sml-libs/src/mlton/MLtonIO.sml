signature MLTON_IO = sig
  type instream
  type outstream
  val inFd : instream -> Posix.IO.file_desc
  val mkstemp : string -> string * outstream
  val mkstemps : {prefix : string, suffix : string} -> string * outstream
  val newIn : Posix.IO.file_desc * string -> instream
  val newOut : Posix.IO.file_desc * string -> outstream
  val outFd : outstream -> Posix.IO.file_desc
  val tempPrefix : string -> string
end
