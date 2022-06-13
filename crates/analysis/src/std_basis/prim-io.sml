signature PRIM_IO = sig
  type elem
  type vector
  type vector_slice
  type array
  type array_slice
  eqtype pos
  val compare : pos * pos -> order
  datatype reader = RD of {
    name : string,
    chunkSize : int,
    readVec : (int -> vector) option,
    readArr : (array_slice -> int) option,
    readVecNB : (int -> vector option) option,
    readArrNB : (array_slice -> int option) option,
    block : (unit -> unit) option,
    canInput : (unit -> bool) option,
    avail : unit -> int option,
    getPos : (unit -> pos) option,
    setPos : (pos -> unit) option,
    endPos : (unit -> pos) option,
    verifyPos : (unit -> pos) option,
    close : unit -> unit,
    ioDesc : OS.IO.iodesc option
  }
  datatype writer = WR of {
    name : string,
    chunkSize : int,
    writeVec : (vector_slice -> int) option,
    writeArr : (array_slice -> int) option,
    writeVecNB : (vector_slice -> int option) option,
    writeArrNB : (array_slice -> int option) option,
    block : (unit -> unit) option,
    canOutput : (unit -> bool) option,
    getPos : (unit -> pos) option,
    setPos : (pos -> unit) option,
    endPos : (unit -> pos) option,
    verifyPos : (unit -> pos) option,
    close : unit -> unit,
    ioDesc : OS.IO.iodesc option
  }
  val openVector : vector -> reader
  val nullRd : unit -> reader
  val nullWr : unit -> writer
  val augmentReader : reader -> reader
  val augmentWriter : writer -> writer
end

structure BinPrimIO :> PRIM_IO
  where type array = Word8Array.array
  where type vector = Word8Vector.vector
  where type elem = Word8.word
  where type pos = Position.int = struct end

structure TextPrimIO :> PRIM_IO
  where type array = CharArray.array
  where type vector = CharVector.vector
  where type elem = Char.char = struct end

structure WideTextPrimIO :> PRIM_IO (* OPTIONAL *)
  where type array = WideCharArray.array
  where type vector = WideCharVector.vector
  where type elem = WideChar.char = struct end
