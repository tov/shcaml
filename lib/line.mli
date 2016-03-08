(** Structured records for line-oriented data.

    This module defines a {!Line.t} type, useful to represent line-oriented data
    with structured content. A {!Line.t} can be thought as a (extensible)
    record, its multiple fields storing the structured data, alongside the raw
    content of the line, from which they are extracted.

    Additionnaly, a line must contain at least the following fields (see
    {!section:mandatory_fields} for more details): [Raw], [Show], [Source],
    [Seq], [After] and [Before].
*)

(** Extensible type for field labels. 

    Field identifiers, or labels, are constructors of the {!label} type, usually
    of arity zero. Several labels are predefined in this module, and more can be
    created by extending the {!label} variant type.

    For example, one can define labels [Foo] and [Bar] using [type label += Foo
    | Bar]. *)
type label = ..

(** When defining a new label, it is expected that a conversion to string is
    registered for it. It will be used by printers, and can be queried using
    {!string_of_label}. *)
val add_label_string : label -> string -> unit

(** Query the string associated with a label. Raises [Failure "string_of_label"]
    if such string has not been registered. *)
val string_of_label : label -> string


(** A mixmap, with {!label}s as keys. We use it to define the type of a
    structured line. *)
module Map : Mixmap.S with type key = label

(** The type of a structured line. 

    A line is a mixmap, with {!label}s as keys, which must contain the fields
    described in section {!section:mandatory_fields}. *)
type t = Map.t

(** Construct a line from a string. 

    [after] is the trailing delimiter data associated with the line (default =
    ["\n"]); [before] is the leading delimiter data associated with the line
    (default = [""]).
*)
val line : ?after: string -> ?before: string -> string -> t

(** {1:mandatory_fields Mandatory fields} *)

type label += Raw | Show | Source | Seq | After | Before

(** The raw string data from which a line was constructed.

    This should typically not change after a line is created; to change the
    principal string value of a line, see {!select} and {!show}. (accessor,
    required) *)
val raw : t -> string

(** Updater for {!raw} *)
val set_raw : string -> t -> t

(** The principal string representation of a line.

    Unlike {!raw} above, the value of {!show} should reflect the data of concern
    in a line.  Use {!set_show} or {!select} to change the value returned by
    {!show}.  When a line is passed to an external process, {!show} is used to
    get a string to send. *)
val show : t -> string

(** Choose the principal string value to be returned by {!show}. *)
val set_show : string -> t -> t

(** Choose the principal string value, using a function.
    [Line.select sel ln] is equivalent to [Line.set_show (sel ln) ln]. *)
val select : (t -> string) -> t -> t

(** Type for the source of a line. *)
type source = [
  | `File of string
  | `Command of string
  | `Process of Proc.execspec
  | `Directory of string
  | `Other of string
  | `None
]

(** The source of a line. (accessor, nullable) *)
val source : t -> source

(** Updater for {!source} *)
val set_source : source -> t -> t

(** The position of a line in its source. (accessor, nullable) *)
val seq : t -> int

(** Updater for {!seq} *)
val set_seq : int -> t -> t


(** Trailing delimiter data associated with a line (accessor, required, default = ["\n"]) *)
val after : t -> string

(** Updater for {!after} *)
val set_after : string -> t -> t


(** Leading delimiter data associated with a line (accessor, required, default = [""]) *)
val before : t -> string

(** Updater for {!before} *)
val set_before : string -> t -> t

(** {1 Utility definitions over [Mixmap] } *)

module Field : sig
  exception Not_found of label

  val get : inj:'a Mixmap.injection -> t -> label -> 'a
  (** Similar to {!Mixmap.find}, but [get ~inj line lbl] raises [Not_found lbl]
      if the key [lbl] is not found, or if its value doesn't belong to the right
      type. *)

  val get_opt : inj:'a Mixmap.injection -> t -> label -> 'a option
  (** Similar to {!Mixmap.get}. *)

  val set : inj:'a Mixmap.injection -> label -> 'a -> t -> t
  (** Similar to {!Mixmap.add}. *)
  
  (** {2 Injections for base types} *)
  val string : string Mixmap.injection
  val int : int Mixmap.injection
  val string_array : string array Mixmap.injection
  val string_list : string list Mixmap.injection
  val source : source Mixmap.injection
end

(** {1 Line structures}

    Several structures that may be read from a line. If the raw contents of a
    line matches a structure corresponding to module [Foo], [Foo.create] will
    parse it and add the corresponding fields to the line. *)

(** Line structure to represent associations of keys with values. *)
module Key_value : sig

  (** The key of a key-value pair. (accessor, required) *)
  val key : t -> string

  (** Updater for {!key} *)
  val set_key : string -> t -> t

  (** The value of a key-value pair. (accessor, required) *)
  val value : t -> string

  (** Updater for {!value} *)
  val set_value : string -> t -> t

  (** The value of the key-value pair, as an integer. *)
  val as_int : t -> int option

  (** The value of the key-value pair, as an float. *)
  val as_float : t -> float option

  (** The value of the key-value pair, as a boolean.

      The strings [yes], [y], [1], [true], [on],
      [enabled], and [enable] are all considered true.
      The strings [no], [n], [0], [false], [off],
      [disabled], and [disable] are all considered false.
      (Case insensitive.) *)
  val as_bool : t -> bool option

  (** The value of the string with interpreted escapes. 

      Backslash escapes are interpreted in the style of {b echo}(1).  The
      optional argument [?quoted] (default [true]) specifies whether to remove
      double-quotes from the string as well (unless they are escaped). *)
  val as_string : ?quoted:bool -> t -> string

  (** The value, split into a list. 

      The optional argument [?delim] (default [' ']) specifies the list
      separator. *)
  val as_list : ?delim:char -> t -> string list

  (** The section in a key-value file. 

      For example, in SSH configuration files, {i Host} statements begin a new
      section. (accessor, nullable) *)
  val section : t -> string

  (** Updater for {!section} *)
  val set_section : string -> t -> t

  (** Add the {!Key_value} substructure to a line. *)
  val create : key:string -> value:string -> t -> t
end

(** Line structure to represent records with delimited fields. 

    For example, this may be appropriate for generic comma- or tab-delimited
    data. *)
module Delim : sig

  (** The fields of a delimited data record (accessor, required, default = [[|  |]]) *)
  val fields : t -> string array

  (** Updater for {!fields} *)
  val set_fields :
    string array -> t -> t

  (** The names of the fields, in order.  This is used by {!get} (accessor, nullable) *)
  val names : t -> string list

  (** Updater for {!names} *)
  val set_names : string list -> t -> t

  val get : string -> t -> string
  val get_int : string -> t -> int
  val get_float : string -> t -> float

  (** The {!Delimited} options with which the record was split (accessor, nullable) *)
  val options : t -> Delimited.options

  (** Updater for {!options} *)
  val set_options : Delimited.options -> t -> t

  (** Output a delimited text file record using the stored
      {!Delimited.options}. *)
  val output : out_channel -> t -> unit

  (** Add the {!Delim} substructure to a line. *)
  val create : fields:string array -> t -> t
end

(** Line structure for {i /etc/passwd} file records. *)
module Passwd : sig
  
  (** The username of a password record. (accessor, required) *)
  val name : t -> string

  (** Updater for {!name} *)
  val set_name : string -> t -> t

  (** The password of a password record. 

      (On modern systems this is usually a dummy marker rather than the
      encrypted password.) (accessor, required) *)
  val passwd : t -> string

  (** Updater for {!passwd} *)
  val set_passwd : string -> t -> t


  (** The user's id in a password record. (accessor, required) *)
  val uid : t -> int

  (** Updater for {!uid} *)
  val set_uid : int -> t -> t


  (** The user's group id in a password record. (accessor, required) *)
  val gid : t -> int

  (** Updater for {!gid} *)
  val set_gid : int -> t -> t


  (** The user's gecos (real name, office, etc.) data in a password
       record. (accessor, required) *)
  val gecos : t -> string

  (** Updater for {!gecos} *)
  val set_gecos : string -> t -> t


  (** The user's home directory in a password record. (accessor, required) *)
  val home : t -> string

  (** Updater for {!home} *)
  val set_home : string -> t -> t

  (** The user's shell in a password record. (accessor, required) *)
  val shell : t -> string

  (** Updater for {!shell} *)
  val set_shell : string -> t -> t

  (** Add the {!Passwd} substructure to a line. *)
  val create :
    name: string ->
    passwd: string ->
    uid: int ->
    gid: int ->
    gecos: string ->
    home: string ->
    shell: string -> t -> t
end

(** Line structure for {i /etc/group} file records. *)
module Group : sig

  (** The group name of a group record. (accessor, required) *)
  val name : t -> string

  (** Updater for {!name} *)
  val set_name : string -> t -> t


  (** The group password name in a group record. (accessor, required) *)
  val passwd : t -> string

  (** Updater for {!passwd} *)
  val set_passwd : string -> t -> t


  (** The group id of a group record. (accessor, required) *)
  val gid : t -> int

  (** Updater for {!gid} *)
  val set_gid : int -> t -> t


  (** The usernames of the members of the group. (accessor, required) *)
  val users : t -> string list

  (** Updater for {!users} *)
  val set_users : string list -> t -> t

  (** Add the {!Group} substructure to a line. *)
  val create :
    name: string ->
    passwd: string ->
    gid: int ->
    users: string list -> t -> t
end

(** Line structure for file status and mode information.  See
    {b stat}(2) and [Unix.stat]. *)
module Stat : sig

  (** The id of the device containing the file. (accessor, required) *)
  val dev : t -> int

  (** Updater for {!dev} *)
  val set_dev : int -> t -> t


  (** The inode of the file. (accessor, required) *)
  val inode : t -> int

  (** Updater for {!inode} *)
  val set_inode : int -> t -> t


  (** The type of the file (regular file, directory, named pipe, etc.).
      See [Unix.file_kind]. (accessor, required, default = [Unix.S_REG]) *)
  val kind : t -> Unix.file_kind

  (** Updater for {!kind} *)
  val set_kind : Unix.file_kind -> t -> t

  (** Various bits in a file's mode (permissions).  See {b chmod}(2). *)
  module Mode : sig

    (** File is executable by owner. (accessor, required) *)
    val xusr : t -> bool

    (** Updater for {!xusr} *)
    val set_xusr : bool -> t -> t


    (** File is writeable by owner. (accessor, required) *)
    val wusr : t -> bool

    (** Updater for {!wusr} *)
    val set_wusr : bool -> t -> t


    (** File is readable by owner. (accessor, required) *)
    val rusr : t -> bool

    (** Updater for {!rusr} *)
    val set_rusr : bool -> t -> t


    (** File is executable by group. (accessor, required) *)
    val xgrp : t -> bool

    (** Updater for {!xgrp} *)
    val set_xgrp : bool -> t -> t


    (** File is writeable by group. (accessor, required) *)
    val wgrp : t -> bool

    (** Updater for {!wgrp} *)
    val set_wgrp : bool -> t -> t


    (** File is readable by group. (accessor, required) *)
    val rgrp : t -> bool

    (** Updater for {!rgrp} *)
    val set_rgrp : bool -> t -> t


    (** File is executable by everyone. (accessor, required) *)
    val xoth : t -> bool

    (** Updater for {!xoth} *)
    val set_xoth : bool -> t -> t


    (** File is writeable by everyone. (accessor, required) *)
    val woth : t -> bool

    (** Updater for {!woth} *)
    val set_woth : bool -> t -> t


    (** File is readable by everyone. (accessor, required) *)
    val roth : t -> bool

    (** Updater for {!roth} *)
    val set_roth : bool -> t -> t


    (** File has set-user-id bit set. (accessor, required) *)
    val suid : t -> bool

    (** Updater for {!suid} *)
    val set_suid : bool -> t -> t


    (** File has set-group-id bit set. (accessor, required) *)
    val sgid : t -> bool

    (** Updater for {!sgid} *)
    val set_sgid : bool -> t -> t


    (** File has sticky bit set. (accessor, required) *)
    val sticky : t -> bool

    (** Updater for {!sticky} *)
    val set_sticky : bool -> t -> t


    (** The entire mode word, as used by {i chmod}. (accessor, required) *)
    val bits : t -> int

    (** Updater for {!bits} *)
    val set_bits : int -> t -> t

    (** Add the {!Mode} substructure to a line. *)
    val create :
      xusr: bool ->
      wusr: bool ->
      rusr: bool ->
      xgrp: bool ->
      wgrp: bool ->
      rgrp: bool ->
      xoth: bool ->
      woth: bool ->
      roth: bool ->
      suid: bool ->
      sgid: bool ->
      sticky: bool ->
      bits: int -> t -> t
  end

  (** The number of hard links to the file. (accessor, required) *)
  val nlink : t -> int

  (** Updater for {!nlink} *)
  val set_nlink : int -> t -> t


  (** User id of the owner of the file. (accessor, required) *)
  val uid : t -> int

  (** Updater for {!uid} *)
  val set_uid : int -> t -> t


  (** Group id of the file. (accessor, required) *)
  val gid : t -> int

  (** Updater for {!gid} *)
  val set_gid : int -> t -> t


  (** Device number of the file (if it's a special file). (accessor, required) *)
  val rdev : t -> int

  (** Updater for {!rdev} *)
  val set_rdev : int -> t -> t


  (** Size of the file in bytes.  This is the size of the file's
      contents for a regular file, or the length of the target path of a
      symbolic link. (accessor, required) *)
  val size : t -> int

  (** Updater for {!size} *)
  val set_size : int -> t -> t


  (** The {i preferred} block size for efficient I/O with this file. (accessor, nullable) *)
  val blksize : t -> int

  (** Updater for {!blksize} *)
  val set_blksize : int -> t -> t

  (** The number of 512-byte blocks in the file. (accessor, nullable) *)
  val blocks : t -> int

  (** Updater for {!blocks} *)
  val set_blocks : int -> t -> t

  (** The file's atime. (accessor, required) *)
  val atime : t -> float

  (** Updater for {!atime} *)
  val set_atime : float -> t -> t


  (** The file's mtime. (accessor, required) *)
  val mtime : t -> float

  (** Updater for {!mtime} *)
  val set_mtime : float -> t -> t


  (** The file's ctime. (accessor, required) *)
  val ctime : t -> float

  (** Updater for {!ctime} *)
  val set_ctime : float -> t -> t

  (** Add the {!Stat} substructure to a line. *)
  val create :
    dev: int ->
    inode: int ->
    kind: Unix.file_kind ->
    nlink: int ->
    uid: int ->
    gid: int ->
    rdev: int ->
    size: int ->
    atime: float ->
    mtime: float ->
    ctime: float -> t -> t
end

(** Line structure for process status information.  See {b ps}(1);
    this corresponds to the {i auxww} option. *)
module Ps : sig

  (** The user running the process. (accessor, required) *)
  val user : t -> string

  (** Updater for {!user} *)
  val set_user : string -> t -> t


  (** The process's id. (accessor, required) *)
  val pid : t -> int

  (** Updater for {!pid} *)
  val set_pid : int -> t -> t


  (** CPU usage. (accessor, required) *)
  val pcpu : t -> float

  (** Updater for {!pcpu} *)
  val set_pcpu : float -> t -> t


  (** Memory usage. (accessor, required) *)
  val pmem : t -> float

  (** Updater for {!pmem} *)
  val set_pmem : float -> t -> t


  (** VM size in KB. (accessor, required) *)
  val vsz : t -> int

  (** Updater for {!vsz} *)
  val set_vsz : int -> t -> t


  (** Memory in residence. (accessor, required) *)
  val rss : t -> int

  (** Updater for {!rss} *)
  val set_rss : int -> t -> t


  (** Terminal number of controlling terminal. (accessor, required) *)
  val tt : t -> string

  (** Updater for {!tt} *)
  val set_tt : string -> t -> t


  (** Process status. (accessor, required) *)
  val stat : t -> string

  (** Updater for {!stat} *)
  val set_stat : string -> t -> t


  (** Time the process was started. (accessor, required) *)
  val started : t -> string

  (** Updater for {!started} *)
  val set_started : string -> t -> t


  (** Time the process has been running. (accessor, required) *)
  val time : t -> string

  (** Updater for {!time} *)
  val set_time : string -> t -> t


  (** Complete invocation of the process. (accessor, required) *)
  val command : t -> string

  (** Updater for {!command} *)
  val set_command : string -> t -> t

  (** Add the {!Ps} substructure to a line. *)
  val create :
    user: string ->
    pid: int ->
    pcpu: float ->
    pmem: float ->
    vsz: int ->
    rss: int ->
    tt: string ->
    stat: string ->
    started: string ->
    time: string ->
    command: string -> t -> t
end

(** Line structure for the file system table {i /etc/fstab}
    records.  See {b fstab}(5). *)
module Fstab : sig

  (** The filesystem to mount. (accessor, required) *)
  val file_system : t -> string

  (** Updater for {!file_system} *)
  val set_file_system : string -> t -> t


  (** The mount point for the filesystem. (accessor, required) *)
  val mount_point : t -> string

  (** Updater for {!mount_point} *)
  val set_mount_point : string -> t -> t


  (** The filesystem type (accessor, required) *)
  val fstype : t -> string

  (** Updater for {!fstype} *)
  val set_fstype : string -> t -> t


  (** Mount options (e.g. ro, user, noauto) (accessor, required) *)
  val options : t -> string list

  (** Updater for {!options} *)
  val set_options : string list -> t -> t


  (** Used by {b dump}(8) to determine which filesystem to dump. (accessor, required) *)
  val dump : t -> int

  (** Updater for {!dump} *)
  val set_dump : int -> t -> t


  (** Pass on which to mount this filesystem (accessor, required) *)
  val pass : t -> int

  (** Updater for {!pass} *)
  val set_pass : int -> t -> t

  (** Add the {!Fstab} substructure to a line. *)
  val create :
    file_system: string ->
    mount_point: string ->
    fstype: string ->
    options: (string list) ->
    dump: int ->
    pass: int -> t -> t
end

(** Line structure for mailcap entries.  See {b mailcap}(5). *)
module Mailcap : sig

  (** The content type to which this mailcap entry applies. (accessor, required) *)
  val content_type : t -> string

  (** Updater for {!content_type} *)
  val set_content_type : string -> t -> t


  (** The command to view files of this type. (accessor, required) *)
  val command : t -> string

  (** Updater for {!command} *)
  val set_command : string -> t -> t


  (** Mailcap flags such as {i needsterminal} or {i copiousoutput}. (accessor, required) *)
  val flags : t -> string list

  (** Updater for {!flags} *)
  val set_flags : string list -> t -> t


  (** Association list of named fields such as {i print=} or {i compose=}. (accessor, required, default = [[]]) *)
  val fields : t -> (string * string) list

  (** Updater for {!fields} *)
  val set_fields : (string * string) list -> t -> t

  (** Add the {!Mailcap} substructure to a line. *)
  val create :
    content_type: string ->
    command: string ->
    flags: string list ->
    fields: ((string * string) list) -> t -> t
end
