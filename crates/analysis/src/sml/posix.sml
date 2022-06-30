(*!
This optional structure contains several substructures that are useful for interfacing to POSIX
operating systems. For more complete information on the semantics of the types and functions
provided in Posix, see the POSIX standard 1003.1,1996 document[CITE].
!*)
signature POSIX (* OPTIONAL *) = sig
  (*!
  Errors and their descriptions.
  !*)
  structure Error : POSIX_ERROR
  (*!
  Signal values and their associated numbers.
  !*)
  structure Signal : POSIX_SIGNAL
  (*!
  Processes: fork, exec, wait, exit, kill, alarm, pause, sleep.
  !*)
  structure Process : POSIX_PROCESS
    where type signal = Signal.signal
  (*!
  User and group IDs, process times, environment, etc.
  !*)
  structure ProcEnv : POSIX_PROC_ENV
    where type pid = Process.pid
  (*!
  File system: open, chdir, chmod, directories, etc.
  !*)
  structure FileSys : POSIX_FILE_SYS
    where type file_desc = ProcEnv.file_desc
    where type uid = ProcEnv.uid
    where type gid = ProcEnv.gid
  (*!
  Input/output: read, write, pipe, dup, close, lock, seek, sync, etc.
  !*)
  structure IO : POSIX_IO
    where type pid = Process.pid
    where type file_desc = ProcEnv.file_desc
    where type open_mode = FileSys.open_mode
  (*!
  Password database, group database, etc.
  !*)
  structure SysDB : POSIX_SYS_DB
    where type uid = ProcEnv.uid
    where type gid = ProcEnv.gid
  (*!
  Terminal (TTY) control: speed, attributes, drain, flush, etc.
  !*)
  structure TTY : POSIX_TTY
    where type pid = Process.pid
    where type file_desc = ProcEnv.file_desc
end

structure Posix :> POSIX (* OPTIONAL *) = struct end
