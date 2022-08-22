(*!
 * The signature TEXT_STREAM_IO extends the STREAM_IO signature to accommodate text I/O. In
 * particular, it binds the I/O element to Char.char, and provides several text-based I/O
 * operations.
 *)
signature TEXT_STREAM_IO = sig
  include STREAM_IO
    where type vector = CharVector.vector
    where type elem = Char.char
  (*!
   * inputLine strm returns SOME(ln, strm'), where ln is the next line of input in the stream strm
   * and strm' is the residual stream. Specifically, ln returns all characters from the current
   * position up to and including the next newline (#"\n") character. If it detects an end-of-stream
   * before the next newline, it returns the characters read appended with a newline. Thus, ln is
   * guaranteed to always be new-line terminated (and thus nonempty). If the current stream position
   * is the end-of-stream, then it returns NONE. It raises Size if the length of the line exceeds
   * the length of the longest string.
   *)
  val inputLine : instream -> (string * instream) option
  (*!
   * outputSubstr (strm, ss) outputs the substring ss to the text stream strm. This is equivalent
   * to: output (strm, Substring.string ss)
   *)
  val outputSubstr : outstream * substring -> unit
end
