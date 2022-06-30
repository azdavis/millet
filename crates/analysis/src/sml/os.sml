(*!
The OS structure is a container for a collection of structures for interacting with the operating
system's file system, directory paths, processes, and I/O subsystem. The types and functions
provided by the OS substructures are meant to present a model for handling these resources that is
largely independent of the operating system.

The structure also declares the SysErr exception used to report operating system error conditions.
!*)
signature OS = sig
  (*!
  File system: files and directories and their attributes.
  !*)
  structure FileSys : OS_FILE_SYS
  (*!
  I/O polling.
  !*)
  structure IO : OS_IO
  (*!
  Syntactic manipulation of pathnames.
  !*)
  structure Path : OS_PATH
  (*!
  Process control, exit status, and environment.
  !*)
  structure Process : OS_PROCESS
  eqtype syserror
  (*!
  This exception is raised when a call to the runtime system or host operating system results in an
  error. The first argument is a descriptive string explaining the error, and the second argument
  optionally specifies the system error condition. The form and content of the description strings
  are operating system and implementation dependent, but if a SysErr exception has the form
  SysErr(s,SOME e), then we have errorMsg e = s. System errors that do not have corresponding
  syserror value will result in SysErr being raised with a second argument of NONE.
  !*)
  exception SysErr of string * syserror option
  (*!
  errorMsg err returns a string describing the system error identified by the error code err. The
  form and content of the description strings are operating system and implementation dependent.
  !*)
  val errorMsg : syserror -> string
  (*!
  These functions provide conversions between the abstract syserror type, and their operating system
  dependent string names. The primary purpose of these functions is to provide a mechanism for
  dealing with error codes that might not have symbolic names defined for them in the operating
  system specific modules. The former function returns a unique name used for the syserror value,
  while the latter returns the syserror whose name is s, if it exists. If e is a syserror, then it
  should be the case that SOME e = syserror(errorName e)
  !*)
  val errorName : syserror -> string
  (*!
  See errorName.
  !*)
  val syserror : string -> syserror option
end

structure OS :> OS = struct end
