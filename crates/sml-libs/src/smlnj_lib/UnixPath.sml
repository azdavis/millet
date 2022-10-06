signature UNIX_PATH = sig
  type path_list = string list
  val getPath : unit -> path_list
  datatype access_mode =
  datatype OS.FileSys.access_mode
  datatype file_type = F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK
  val findFile : (path_list * access_mode list) -> string -> string option
  val findFiles : (path_list * access_mode list) -> string -> string list
  val findFileOfType : (path_list * file_type * access_mode list) -> string -> string option
  val findFilesOfType : (path_list * file_type * access_mode list) -> string -> string list
end

structure UnixPath : UNIX_PATH = struct end
