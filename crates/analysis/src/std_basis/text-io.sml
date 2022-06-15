signature TEXT_IO = sig
  include IMPERATIVE_IO
  structure StreamIO : TEXT_STREAM_IO
    where type reader = TextPrimIO.reader
    where type writer = TextPrimIO.writer
    where type pos = TextPrimIO.pos
  val inputLine : instream -> string option
  val outputSubstr : outstream * Substring.substring -> unit
  val openIn : string -> instream
  val openOut : string -> outstream
  val openAppend : string -> outstream
  val openString : string -> instream
  val stdIn : instream
  val stdOut : outstream
  val stdErr : outstream
  val print : string -> unit
  val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader -> ('a, StreamIO.instream) StringCvt.reader) -> instream -> 'a option
end

structure TextIO :> TEXT_IO = struct end
structure WideTextIO :> TEXT_IO (* OPTIONAL *) = struct end
