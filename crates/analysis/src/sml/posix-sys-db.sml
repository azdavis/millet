(*!
The Posix.SysDB structure implements operations on the user database and the group database (in
POSIX parlance, the password file and the group file). These are the data and operations described
in Section 9 of the POSIX standard 1003.1,1996[CITE].
!*)
signature POSIX_SYS_DB = sig
  (*!
  User identifier; identical to Posix.ProcEnv.uid.
  !*)
  eqtype uid
  (*!
  Group identifier; identical to Posix.ProcEnv.gid.
  !*)
  eqtype gid
  structure Passwd : sig
    (*!
    Information related to a user.
    !*)
    type passwd
    (*!
    These extract the name, the user ID, the group ID, the path of the initial working, or home,
    directory, and the initial command shell, respectively, of the user corresponding to the passwd
    value. The names of the corresponding fields in C are the same, but prefixed with "pw_". The one
    exception is that C uses pw_dir for the home directory.
    !*)
    val name : passwd -> string
    val uid : passwd -> uid
    val gid : passwd -> gid
    val home : passwd -> string
    val shell : passwd -> string
  end
  (*!
  type group  val name : group -> string val gid : group -> gid val
  members : group -> string list
  !*)
  structure Group : sig
    (*!
    Information related to a group.
    !*)
    type group
    (*!
    These extract the name, the group ID, and the names of users belonging to the group,
    respectively, of the group corresponding to the group value. In C, these fields are named
    gr_name, gr_gid, and gr_mem, respectively.
    !*)
    val name : group -> string
    val gid : group -> gid
    val members : group -> string list
  end
  (*!
  These return the group or user database entry associated with the given group ID or name, or user
  ID or name. It raises OS.SysErr if there is no group or user with the given ID or name.
  !*)
  val getgrgid : gid -> Group.group
  val getgrnam : string -> Group.group
  val getpwuid : uid -> Passwd.passwd
  val getpwnam : string -> Passwd.passwd
end

structure SysDB : POSIX_SYS_DB = struct end
