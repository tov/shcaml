(*
 * vim: ft=ocaml sw=2 :
 *)

with "Structured records for line-oriented data"

let default: float              = 0.0
let default: string option      = None
let default: int option         = None
let default: string list        = []

val !raw: string
  with "The raw string data from which a line was constructed.  This
should typically not change after a line is created; to change the
principal string value of a line, see {!select} and {!show}."

type .stringifier = { stringifier : 'a . 'a t -> string }
val .stringifier: stringifier = { stringifier = raw }

let show : 'a t -> string =
  fun r -> (stringifier r).stringifier r
with "The principal string representation of a line.
Unlike {!raw} above, the value of {!show} should reflect the
data of concern in a line.  Use {!set_show} or {!select} to change
the value returned by {!show}.  When a line is passed to an external
process, {!show} is used to get a string to send."

let set_show : string -> 'a t -> 'a t =
  fun s r -> set_stringifier { stringifier = fun _ -> s } r
with "Choose the principal string value to be returned by {!show}."

let select : ('a t -> string) -> 'a t -> 'a t
  = fun f r ->
      set_stringifier { stringifier = fun _ -> f r } r
with "Choose the principal string value, using a function.
[Line.select sel ln] is equivalent to [Line.set_show (sel ln) ln]."

type source = [
  | `File      of string
  | `Command   of string
  | `Process   of Proc.execspec
  | `Directory of string
  | `Other     of string
  | `None
]
with "Type for the source of a line. See {!val: source}."
let default: source = `None

val source: source
  with "The source of a line."
val seq: int
  with "The position of a line in its source."

(* Key-value options to consider:
 * separator (:, =, space, :=)
 * parse values (int, bool)
 * quoting?
 * spaces?
 * comments?
 * sections? ([], <>, Host:, {, nesting?)
 *)
module Key_value = struct
  with "Line structure to represent associations of keys with values."

  val ~key: string
  with "The key of a key-value pair."
  val ~value: string
  with "The value of a key-value pair."

  let as_int: < key_value: < .. >; .. > t -> int option =
    fun line ->
      try Some (int_of_string (value line)) with
      | Failure _ -> None
  with "The value of the key-value pair, as an integer."

  let as_float: < key_value: < .. >; .. > t -> float option =
    fun line ->
      try Some (float_of_string (value line)) with
      | Failure _ -> None
  with "The value of the key-value pair, as an float."

  let as_bool: < key_value: < .. >; .. > t -> bool option =
    fun line ->
      match String.lowercase (value line) with
      | "yes" | "y" | "1" | "true"  | "on"  | "enabled"  | "enable" ->
          Some true
      | "no"  | "n" | "0" | "false" | "off" | "disabled" | "disable" ->
          Some false
      | _ ->
          None
  with "The value of the key-value pair, as a boolean.
        The strings [yes], [y], [1], [true], [on],
        [enabled], and [enable] are all considered true.
        The strings [no], [n], [0], [false], [off],
        [disabled], and [disable] are all considered false.
        (Case insensitive.)"

  let as_string: ?quoted:bool -> < key_value: < .. >; .. > t -> string =
    fun ?(quoted = true) line ->
      match Delimited.splitter ~options:
        { Delimited.default_options with
            Delimited.max_fields        = 1;
            Delimited.rec_quotation     = quoted;
            Delimited.rec_escapes       = true;
            Delimited.rec_double_double = false; }
        (value line) with
      | [| str |] -> str
      | _         -> raise Util.Bug
  with
  "The value of the string with interpreted escapes.  Backslash escapes
   are interpreted in the style of {b echo}(1).  The optional argument
   [?quoted] (default [true]) specifies whether to remove double-quotes
   from the string as well (unless they are escaped)."

  let as_list: ?delim:char -> < key_value: < .. >; .. > t -> string list =
    fun ?(delim = ' ') line ->
      Array.to_list
        (Delimited.splitter ~options:
          { Delimited.default_options with
              Delimited.field_sep = delim;
              Delimited.rec_quotation = false; }
          (value line))
  with "The value, split into a list.  The optional argument
        [?delim] (default [' ']) specifies the list separator."

  val section: string
  with
  "The section in a key-value file.  For example, in SSH configuration
   files, {i Host} statements begin a new section."
end

module Delim = struct
  with "Line structure to represent records with delimited fields.  For
        example, this may be appropriate for generic comma- or tab-delimited
        data."

  val ~fields: string array = [| |]
  with "The fields of a delimited data record"

  val names: string list
  with "The names of the fields, in order.  This is used by {!get}"

  let get:
    string -> < delim: < names: present; .. >; .. > t -> string =
    let rec index i item = function
      | [] -> None
      | x :: _ when x = item -> Some i
      | _ :: xs -> index (i + 1) item xs in
    fun key line ->
      match index 0 key (names line) with
      | None   -> raise Not_found
      | Some i -> (fields line).(i)

  let get_int:
    string -> < delim: < names: present; .. >; .. > t -> int =
    fun key line -> int_of_string (get key line)

  let get_float:
    string -> < delim: < names: present; .. >; .. > t -> float =
    fun key line -> float_of_string (get key line)

  val options: Delimited.options = Delimited.default_options
  with "The {!Delimited} options with which the record was split"

  let output:
    out_channel -> < delim: < options: present; .. >; .. > t -> unit =
      fun channel line ->
        Delimited.output_record channel ~options:(options line) (fields line)
  with "Output a delimited text file record using the stored
        {!Delimited.options}."
end

module Passwd = struct
  with "Line structure for {i /etc/passwd} file records."

  val ~name: string
    with "The username of a password record."

  val ~passwd: string
    with "The password of a password record.  (On modern systems this is
          usually a dummy marker rather than the encrypted password.)"
  val ~uid: int
    with "The user's id in a password record."
  val ~gid: int
    with "The user's group id in a password record."
  val ~gecos: string
    with "The user's gecos (real name, office, etc.) data in a
          password record."
  val ~home: string
    with "The user's home directory in a password record."
  val ~shell: string
    with "The user's shell in a password record."
end

module Group = struct
  with "Line structure for {i /etc/group} file records."

  val ~name: string
    with "The group name of a group record."
  val ~passwd: string
    with "The group password name in a group record."
  val ~gid: int
    with "The group id of a group record."
  val ~users: string list
    with "The usernames of the members of the group."
end

module Stat = struct
  with "Line structure for file status and mode information.  See
        {b stat}(2) and [Unix.stat]."
  val ~dev: int
    with "The id of the device containing the file."
  val ~inode: int
    with "The inode of the file."
  val ~kind: Unix.file_kind = Unix.S_REG
    with "The type of the file (regular file, directory, named pipe, etc.).
          See [Unix.file_kind]."

  module Mode = struct
    with "Various bits in a file's mode (permissions).  See {b
          chmod}(2)."
    val ~xusr: bool
      with "File is executable by owner."
    val ~wusr: bool
      with "File is writeable by owner."
    val ~rusr: bool
      with "File is readable by owner."
    val ~xgrp: bool
      with "File is executable by group."
    val ~wgrp: bool
      with "File is writeable by group."
    val ~rgrp: bool
      with "File is readable by group."
    val ~xoth: bool
      with "File is executable by everyone."
    val ~woth: bool
      with "File is writeable by everyone."
    val ~roth: bool
      with "File is readable by everyone."
    val ~suid: bool
      with "File has set-user-id bit set."
    val ~sgid: bool
      with "File has set-group-id bit set."
    val ~sticky: bool
      with "File has sticky bit set."
    val ~bits: int
      with "The entire mode word, as used by {i chmod}."
  end

  val ~nlink: int
    with "The number of hard links to the file."
  val ~uid: int
    with "User id of the owner of the file."
  val ~gid: int
    with "Group id of the file."
  val ~rdev: int
    with "Device number of the file (if it's a special file)."
  val ~size: int
    with "Size of the file in bytes.  This is the size of the file's
         contents for a regular file, or the length of the target path of a
        symbolic link."
  val  blksize: int
    with "The {i preferred} block size for efficient I/O with this file."
  val  blocks: int
    with "The number of 512-byte blocks in the file."
  val ~atime: float
    with "The file's atime."
  val ~mtime: float
    with "The file's mtime."
  val ~ctime: float
    with "The file's ctime."
end

module Ps = struct
  with "Line structure for process status information.  See {b ps}(1);
       this corresponds to the {i auxww} option."
  val ~user: string
    with "The user running the process."
  val ~pid:  int
    with "The process's id."
  val ~pcpu: float
    with "CPU usage."
  val ~pmem: float
    with "Memory usage."
  val ~vsz:  int 
    with "VM size in KB."
  val ~rss:  int 
    with "Memory in residence."
  val ~tt:   string
    with "Terminal number of controlling terminal."
  val ~stat: string
    with "Process status."
  val ~started: string
    with "Time the process was started."
  val ~time:    string
    with "Time the process has been running."
  val ~command: string
    with "Complete invocation of the process."
end

module Fstab = struct
  with "Line structure for the file system table {i /etc/fstab}
        records.  See {b fstab}(5)."
  val ~file_system: string
    with "The filesystem to mount."
  val ~mount_point: string
    with "The mount point for the filesystem."
  val ~fstype: string
    with "The filesystem type"
  val ~options: string list
    with "Mount options (e.g. ro, user, noauto)"
  val ~dump: int
    with "Used by {b dump}(8) to determine which filesystem to dump."
  val ~pass: int
    with "Pass on which to mount this filesystem"
end

module Mailcap = struct
  with "Line structure for mailcap entries.  See {b mailcap}(5)."

  val ~content_type: string
    with "The content type to which this mailcap entry applies."
  val ~command: string
    with "The command to view files of this type."
  val ~flags: string list
    with "Mailcap flags such as {i needsterminal} or {i copiousoutput}."
  val ~fields: (string * string) list = []
    with "Association list of named fields such as {i print=} or {i compose=}."
end

val ?after:  string = "\n"
  with "Trailing delimiter data associated with a line"
val ?before: string = ""
  with "Leading delimiter data associated with a line"
