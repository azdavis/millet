(*!
 * The Windows structure provides a high-level interface to various system features based on the
 * Microsoft Windows operating system model. These functions include the ability to create and
 * communicate with separate processes, as well as to interact with the registry and file
 * subsystems. In particular, using this module, a program can invoke a separate process and obtain
 * input and output streams connected to the standard output and input streams, respectively, of the
 * other process. The functions provide a richer and more detailed interface than the comparable
 * functions provided by the substructures in OS.
 *)
signature WINDOWS (* OPTIONAL *) = sig
  structure Key : sig
    include BIT_FLAGS
    val allAccess : flags
    val createLink : flags
    val createSubKey : flags
    val enumerateSubKeys : flags
    val execute : flags
    val notify : flags
    val queryValue : flags
    val read : flags
    val setValue : flags
    val write : flags
  end
  structure Reg : sig
    eqtype hkey
    val classesRoot : hkey
    val currentUser : hkey
    val localMachine : hkey
    val users : hkey
    val performanceData : hkey
    val currentConfig : hkey
    val dynData : hkey
    datatype create_result = CREATED_NEW_KEY of hkey | OPENED_EXISTING_KEY of hkey
    val createKeyEx : hkey * string * Key.flags -> create_result
    val openKeyEx : hkey * string * Key.flags -> hkey
    val closeKey : hkey -> unit
    val deleteKey : hkey * string -> unit
    val deleteValue : hkey * string -> unit
    val enumKeyEx : hkey * int -> string option
    val enumValueEx : hkey * int -> string option
    datatype value = SZ of string | DWORD of SysWord.word | BINARY of Word8Vector.vector | MULTI_SZ of string list | EXPAND_SZ of string
    val queryValueEx : hkey * string -> value option
    val setValueEx : hkey * string * value -> unit
  end
  structure Config : sig
    val platformWin32s : SysWord.word
    val platformWin32Windows : SysWord.word
    val platformWin32NT : SysWord.word
    val platformWin32CE : SysWord.word
    val getVersionEx : unit -> { majorVersion : SysWord.word, minorVersion : SysWord.word, buildNumber : SysWord.word, platformId : SysWord.word, csdVersion : string }
    val getWindowsDirectory : unit -> string
    val getSystemDirectory : unit -> string
    val getComputerName : unit -> string
    val getUserName : unit -> string
  end
  structure DDE : sig
    type info
    val startDialog : string * string -> info
    val executeString : info * string * int * Time.time -> unit
    val stopDialog : info -> unit
  end
  (*!
   * getVolumeInformation root returns information about the filesystem and volume specified by the
   * root pathname root. The volumeName field contains the name of the volume; the systemName field
   * contains its type (e.g., "FAT" or "NTFS"); the serialNumber field contains the serial number;
   * and the maximumComponentLength field specifies the maximum length of any component of a
   * pathname on this system.
   *)
  val getVolumeInformation :
    string -> {
      volumeName : string,
      systemName : string,
      serialNumber : SysWord.word,
      maximumComponentLength : int
    }
  (*!
   * findExecutable name returns the full executable name associated with name, or NONE if no such
   * file exists.
   *)
  val findExecutable : string -> string option
  (*!
   * launchApplication (file, arg) runs the specified executable file passing it the argument arg.
   * It raises OS.SysErr if file is not executable or if it cannot be run. Implementation note: This
   * should be implemented using ShellExecute, passing SW_SHOWNORMAL to the underlying API call.
   *)
  val launchApplication : string * string -> unit
  (*!
   * openDocument file opens file using its associated application. Implementation note: This should
   * pass SW_SHOWNORMAL to the underlying ShellExecute API call.
   *)
  val openDocument : string -> unit
  (*!
   * simpleExecute (cmd, arg) spawns the process specified by cmd with command-line arguments
   * represented by the string arg, redirecting standard input and standard output to the null
   * device. It then waits for the subprocess to terminate, and returns its exit status. This is
   * similar to OS.Process.system but it can be used in cases where the latter does not work, and
   * its return value provides more information about the exit status of the child process.
   * Implementation note: This corresponds to the use of CreateProcess.
   *)
  val simpleExecute : string * string -> OS.Process.status
  (*!
   * The type of a process created by execute. The type parameters are witness types for the types
   * of streams that can be returned.
   *)
  type ('a,'b) proc
  (*!
   * execute (cmd, arg) spawns a process specified by cmd with command-line arguments represented by
   * the string arg and returns a handle for the resulting process. Implementation note: This also
   * corresponds to the use of CreateProcess. Redirection of the standard streams can be handled
   * using the hStdInput and hStdOutput fields in the STARTUPINFO parameter.
   *)
  val execute : string * string -> ('a, 'b) proc
  (*!
   * These functions return a text or binary instream connected to the standard output stream of the
   * process pr. Note that multiple calls to these functions on the same proc value will result in
   * multiple streams that all share the same underlying open file descriptor, which can lead to
   * unpredictable effects because
   *)
  val textInstreamOf : (TextIO.instream, 'a) proc -> TextIO.instream
  (*!
   * See textInstreamOf.
   *)
  val binInstreamOf : (BinIO.instream, 'a) proc -> BinIO.instream
  (*!
   * These functions return a text or binary outstream connected to the standard input stream of the
   * process pr. Note that multiple calls to these functions on the same proc value will result in
   * multiple streams that all share the same underlying open file descriptor, which can lead to
   * unpredictable effects due to buffering.
   *)
  val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
  (*!
   * See textOutstreamOf.
   *)
  val binOutstreamOf : ('a, BinIO.outstream) proc -> BinIO.outstream
  (*!
   * reap pr closes the standard streams associated with pr, and then suspends the current process
   * until the system process corresponding to pr terminates. It returns the exit status given by pr
   * when it terminated. If reap is applied again to pr, it should immediately return the previous
   * exit status. Implementation note: Typically, one cannot rely on the underlying operating system
   * to provide the exit status of a terminated process after it has done so once. Thus, the exit
   * status probably needs to be cached. Also note that reap should not return until the process
   * being monitored has terminated. In particular, implementations should be careful not to return
   * if the process has only been suspended.
   *)
  val reap : ('a, 'b) proc -> OS.Process.status
  (*!
   * The Status substructure defines the possible system-specific interpretations of
   * OS.Process.status values.
   *)
  structure Status : sig
    type status = SysWord.word
    val accessViolation : status
    val arrayBoundsExceeded : status
    val breakpoint : status
    val controlCExit : status
    val datatypeMisalignment : status
    val floatDenormalOperand : status
    val floatDivideByZero : status
    val floatInexactResult : status
    val floatInvalidOperation : status
    val floatOverflow : status
    val floatStackCheck : status
    val floatUnderflow : status
    val guardPageViolation : status
    val integerDivideByZero : status
    val integerOverflow : status
    val illegalInstruction : status
    val invalidDisposition : status
    val invalidHandle : status
    val inPageError : status
    val noncontinuableException : status
    val pending : status
    val privilegedInstruction : status
    val singleStep : status
    val stackOverflow : status
    val timeout : status
    val userAPC : status
  end
  (*!
   * fromStatus s decodes the abstract exit status s into system-specific information.
   *)
  val fromStatus : OS.Process.status -> Status.status
  (*!
   * exit st executes all actions registered with OS.Process.atExit, flushes and closes all I/O
   * streams, then terminates the SML process with termination status st.
   *)
  val exit : Status.status -> 'a
end

structure Windows :> WINDOWS (* OPTIONAL *) = struct end
