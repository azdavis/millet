(*!
The TEXT_IO interface provides input/output of characters and strings. Most of the operations
themselves are defined in the IMPERATIVE_IO signature.

The TEXT_IO signature is matched by two structures, the required TextIO and the optional WideTextIO.
The former implements strings based on the extended ASCII 8-bit characters. The latter provides
strings of characters of some size greater than or equal to 8 bits.

The signature given below for TEXT_IO is not valid SML, in that the substructure StreamIO is
respecified. (It is initially specified as a substructure having signature STREAM_IO in the included
signature IMPERATIVE_IO.) This abuse of notation seems acceptable in that the intended meaning is
clear (a structure matching TEXT_IO also matches IMPERATIVE_IO and has a substructure StreamIO that
matches TEXT_STREAM_IO) while avoiding a textual inclusion of the whole signature of IMPERATIVE_IO
except its StreamIO substructure.
!*)
signature TEXT_IO = sig
  include IMPERATIVE_IO_HACK
  (*!
  inputLine strm returns SOME(ln), where ln is the next line of input in the stream strm.
  Specifically, ln returns all characters from the current position up to and including the next
  newline (#"\n") character. If it detects an end-of-stream before the next newline, it returns the
  characters read appended with a newline. Thus, ln is guaranteed to always be new-line terminated
  (and thus nonempty). If the current stream position is the end-of-stream, then it returns NONE. It
  raises Size if the length of the line exceeds the length of the longest string.
  !*)
  val inputLine : instream -> string option
  (*!
  outputSubstr (strm, ss) outputs the substring ss to the text stream strm. This is equivalent to:
  output (strm, Substring.string ss)
  !*)
  val outputSubstr : outstream * substring -> unit
  (*!
  These open the file named name for input and output, respectively. If name is a relative pathname,
  the file opened depends on the current working directory. On openOut, the file is created if it
  does not already exist and truncated to length zero otherwise. It raises Io if a stream cannot be
  opened on the given file, or in the case of openIn, the file name does not exist.
  !*)
  val openIn : string -> instream
  (*!
  See openIn.
  !*)
  val openOut : string -> outstream
  (*!
  openAppend name opens the file named name for output in append mode, creating it if it does not
  already exist. If the file already exists, the file pointer is positioned at the end of the file.
  It raises Io if a stream cannot be opened on the given file. Beyond having the initial file
  position be at the end of the file, any additional properties are system and implementation
  dependent. On operating systems (e.g., Unix) that support ``atomic append mode,'' each (flushed)
  output operation to the file will be appended to the end, even if there are other processes
  writing to the file simultaneously. Due to buffering, however, these writes need not be atomic,
  i.e., output from a different process may interleave the output of a single write using the stream
  library. On certain other operating systems, having the file open for writing prevents any other
  process from opening the file for writing.
  !*)
  val openAppend : string -> outstream
  (*!
  openString s creates an input stream whose content is s.
  !*)
  val openString : string -> instream
  (*!
  These correspond to the standard input, output, and error streams, respectively.
  !*)
  val stdIn : instream
  val stdOut : outstream
  val stdErr : outstream
  (*!
  print s prints the string s to the standard output stream and flushes the stream. No newline
  character is appended. This is available in the top-level environment as print. This is equivalent
  to: (output (stdOut, s); flushOut stdOut)
  !*)
  val print : string -> unit
  (*!
  scanStream scanFn strm converts a stream-based scan function into one that works on Imperative I/O
  streams. For example, to attempt to scan a decimal integer from stdIn, one could use scanStream
  (Int.scan StringCvt.DEC) stdIn The function can be implemented as: fun scanStream scanFn strm =
  let val instrm = getInstream strm in case (scanFn StreamIO.input1 instrm) of NONE => NONE |
  SOME(v, instrm') => ( setInstream (strm, instrm'); SOME v) end In addition to providing a
  convenient way to use Stream I/O scanning functions with Imperative I/O, the scanStream assures
  that input is not inadvertently lost due to lookahead during scanning.
  !*)
  val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader -> ('a, StreamIO.instream) StringCvt.reader) -> instream -> 'a option
end

structure TextIO :> TEXT_IO = struct end
structure WideTextIO :> TEXT_IO (* OPTIONAL *) = struct end
val print = TextIO.print
