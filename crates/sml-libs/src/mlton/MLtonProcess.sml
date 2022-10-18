signature MLTON_PROCESS = sig
  type pid
  val spawn : {args : string list, path : string} -> pid
  val spawne : {args : string list, env : string list, path : string} -> pid
  val spawnp : {args : string list, file : string} -> pid
  type ('stdin, 'stdout, 'stderr) t
  type input
  type output
  type none
  type chain
  type any
  exception MisuseOfForget
  exception DoublyRedirected
  structure Child : sig
    type ('use, 'dir) t
    val binIn : (BinIO.instream, input) t -> BinIO.instream
    val binOut : (BinIO.outstream, output) t -> BinIO.outstream
    val fd : (Posix.FileSys.file_desc, 'dir) t -> Posix.FileSys.file_desc
    val remember : (any, 'dir) t -> ('use, 'dir) t
    val textIn : (TextIO.instream, input) t -> TextIO.instream
    val textOut : (TextIO.outstream, output) t -> TextIO.outstream
  end
  structure Param : sig
    type ('use, 'dir) t
    val child : (chain, 'dir) Child.t -> (none, 'dir) t
    val fd : Posix.FileSys.file_desc -> (none, 'dir) t
    val file : string -> (none, 'dir) t
    val forget : ('use, 'dir) t -> (any, 'dir) t
    val null : (none, 'dir) t
    val pipe : ('use, 'dir) t
    val self : (none, 'dir) t
  end
  val create : {args : string list, env : string list option, path : string, stderr : ('stderr, output) Param.t, stdin : ('stdin, input) Param.t, stdout : ('stdout, output) Param.t} -> ('stdin, 'stdout, 'stderr) t
  val getStderr : ('stdin, 'stdout, 'stderr) t -> ('stderr, input) Child.t
  val getStdin : ('stdin, 'stdout, 'stderr) t -> ('stdin, output) Child.t
  val getStdout : ('stdin, 'stdout, 'stderr) t -> ('stdout, input) Child.t
  val kill : ('stdin, 'stdout, 'stderr) t * Posix.Signal.signal -> unit
  val reap : ('stdin, 'stdout, 'stderr) t -> Posix.Process.exit_status
end
