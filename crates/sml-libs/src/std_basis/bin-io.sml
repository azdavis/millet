(*!
 * The structure BinIO provides input/output of binary data (8-bit bytes). The semantics of the
 * various I/O operations can be found in the description of the IMPERATIVE_IO signature. The openIn
 * and openOut functions allow the creation of binary streams to read and write file data. Certain
 * implementations may provide other ways to open files in structures specific to an operating
 * system.
 *)
signature BIN_IO = sig
  include IMPERATIVE_IO
    where type StreamIO.vector = Word8Vector.vector
    where type StreamIO.elem = Word8.word
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
    where type StreamIO.pos = BinPrimIO.pos
  (*!
   * These functions open the file named name for input and output, respectively. If name is a
   * relative pathname, the file opened depends on the current working directory. With openOut, the
   * file is created if it does not already exist and truncated to length zero otherwise. These
   * raise Io if a stream cannot be opened on the given file or, in the case of openIn, the file
   * name does not exist.
   *)
  val openIn : string -> instream
  (*!
   * See openIn.
   *)
  val openOut : string -> outstream
  (*!
   * openAppend name opens the file named name for output in append mode, creating it if it does not
   * already exist. If the file already exists, it sets the current position at the end of the file.
   * It raises Io if a stream cannot be opened on the given file. Beyond having the initial file
   * position at the end of the file, any additional properties are system and implementation
   * dependent. On operating systems (e.g., Unix) that support ``atomic append mode,'' each
   * (flushed) output operation to the file will be appended to the end, even if there are other
   * processes writing to the file simultaneously. Due to buffering, however, writing on an
   * outstream need not be atomic, i.e., output from a different process may interleave the output
   * of a single write using the stream library. On certain other operating systems, having the file
   * open for writing prevents any other process from opening the file for writing.
   *)
  val openAppend : string -> outstream
end

structure BinIO :> BIN_IO = struct end
