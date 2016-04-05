type label = ..

let label_strings = ref []

let string_of_label lbl =
  try List.assoc lbl !label_strings with
    Not_found -> raise (Failure "string_of_label")

let add_label_string lbl str =
  label_strings := (lbl, str) :: !label_strings

exception Field_not_found of label

let _ =
  Printexc.register_printer (function
    | Field_not_found lbl ->
      (try Some (string_of_label lbl)
       with Failure "string_of_label" -> None)
    | _ -> None)

module Fields = struct
  module M = Hmap.Make (struct type 'a t = label end)
  include M

  let get k m = try M.get k m with
    Invalid_argument _ -> raise (Field_not_found (M.Key.info k))
end  

type t = Fields.t

type source = [
  | `File of string
  | `Command of string
  | `Process of Proc.execspec
  | `Directory of string
  | `Other of string
  | `None
]

type label += Raw | Show | Source | Seq | After | Before

let _ =
  add_label_string Raw "raw";
  add_label_string Show "show";
  add_label_string Source "source";
  add_label_string Seq "seq";
  add_label_string After "after";
  add_label_string Before "before"

let raw_k = Fields.Key.create Raw
let raw = Fields.get raw_k
let set_raw = Fields.add raw_k

let show_k = Fields.Key.create Show
let show r = (Fields.get show_k r) ()
let set_show s = Fields.add show_k (fun _ -> s)
let select sel r = Fields.add show_k (fun _ -> sel r) r

let source_k = Fields.Key.create Source
let source = Fields.get source_k
let set_source = Fields.add source_k

let seq_k = Fields.Key.create Seq
let seq = Fields.get seq_k
let set_seq = Fields.add seq_k

let after_k = Fields.Key.create After
let after = Fields.get after_k
let set_after = Fields.add after_k

let before_k = Fields.Key.create Before
let before = Fields.get before_k
let set_before = Fields.add before_k

module Key_value = struct
  type t = { key : string; value : string; section: string }

  type label += Key_value
  let _ = add_label_string Key_value "key_value"

  let kv_k = Fields.Key.create Key_value

  let key r = (Fields.get kv_k r).key
  let set_key key r =
    let t = Fields.get kv_k r in
    Fields.add kv_k { t with key } r

  let value r = (Fields.get kv_k r).value
  let set_value value r =
    let t = Fields.get kv_k r in
    Fields.add kv_k { t with value } r

  let as_int line =
    try Some (int_of_string (value line)) with Failure _ -> None

  let as_float line =
    try Some (float_of_string (value line)) with Failure _ -> None

  let as_bool line =
    match String.lowercase (value line) with
    | "yes" | "y" | "1" | "true" | "on" | "enabled" | "enable" -> Some true
    | "no" | "n" | "0" | "false" | "off" | "disabled" | "disable" ->
      Some false
    | _ -> None

  let as_string ?(quoted = true) line =
    match Delimited.splitter
            ~options:
              {
                (Delimited.
                   default_options)
                with
                  Delimited.max_fields = 1;
                  Delimited.rec_quotation = quoted;
                  Delimited.rec_escapes = true;
                  Delimited.rec_double_double = false;
              }
            (value line)
    with
    | [| str |] -> str
    | _ -> raise Util.Bug
             
  let as_list ?(delim = ' ') line =
    Array.to_list
      (Delimited.splitter
         ~options:{
           Delimited.default_options
           with
             Delimited.field_sep = delim;
             Delimited.rec_quotation = false;
         }
         (value line))

  let section r = (Fields.get kv_k r).section
  let set_section section r =
    let t = Fields.get kv_k r in
    Fields.add kv_k { t with section } r

  let empty = { key = ""; value = ""; section = "" }

  let create ~key ~value r =
    Fields.add kv_k { key; value; section = "" } r
end

module Delim = struct
  type t = { options: Delimited.options;
             names: string list;
             fields: string array }

  type label += Delim
  let _ = add_label_string Delim "delim"

  let delim_k = Fields.Key.create Delim

  let fields r = (Fields.get delim_k r).fields
  let set_fields fields r =
    let t = Fields.get delim_k r in
    Fields.add delim_k { t with fields } r

  let names r = (Fields.get delim_k r).names
  let set_names names r =
    let t = Fields.get delim_k r in
    Fields.add delim_k { t with names } r

  let get =
    let rec index i item =
      function
      | [] -> None
      | x :: _ when x = item -> Some i
      | _ :: xs -> index (i + 1) item xs
    in
    fun key line ->
      match index 0 key (names line) with
      | None -> raise Not_found
      | Some i -> (fields line).(i)

  let get_int key line = int_of_string (get key line)
  let get_float key line = float_of_string (get key line)

  let options r = (Fields.get delim_k r).options
  let set_options options r =
    let t = Fields.get delim_k r in
    Fields.add delim_k { t with options } r

  let output channel line =
    Delimited.output_record channel ~options: (options line) (fields line)

  let empty = {
    fields = [| |];
    names = [];
    options = Delimited.default_options
  }

  let create ~fields r =
    Fields.add delim_k
      { fields; names = []; options = Delimited.default_options } r
end

module Passwd = struct
  type t = {
    shell: string;
    home: string;
    gecos: string;
    gid: int;
    uid: int;
    passwd: string;
    name: string
  }

  type label += Passwd
  let _ = add_label_string Passwd "passwd"

  let passwd_k = Fields.Key.create Passwd

  let name r = (Fields.get passwd_k r).name
  let set_name name r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with name } r

  let passwd r = (Fields.get passwd_k r).passwd
  let set_passwd passwd r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with passwd } r
  
  let uid r = (Fields.get passwd_k r).uid
  let set_uid uid r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with uid } r

  let gid r = (Fields.get passwd_k r).gid
  let set_gid gid r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with gid } r

  let gecos r = (Fields.get passwd_k r).gecos
  let set_gecos gecos r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with gecos } r

  let home r = (Fields.get passwd_k r).home
  let set_home home r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with home } r

  let shell r = (Fields.get passwd_k r).shell
  let set_shell shell r =
    let t = Fields.get passwd_k r in
    Fields.add passwd_k { t with shell } r

  let empty = {
    name = "";
    passwd = "";
    uid = 0; gid = 0;
    gecos = "";
    home = "";
    shell = "";
  }

  let create ~name ~passwd ~uid ~gid ~gecos ~home ~shell r =
    Fields.add passwd_k
      { name; passwd; uid; gid; gecos; home; shell } r
end

module Group = struct
  type t = {
    name: string;
    passwd: string;
    gid: int;
    users: string list
  }

  type label += Group
  let _ = add_label_string Group "group"
  
  let group_k = Fields.Key.create Group

  let name r = (Fields.get group_k r).name
  let set_name name r =
    let t = Fields.get group_k r in
    Fields.add group_k { t with name } r

  let passwd r = (Fields.get group_k r).passwd
  let set_passwd passwd r =
    let t = Fields.get group_k r in
    Fields.add group_k { t with passwd } r

  let gid r = (Fields.get group_k r).gid
  let set_gid gid r =
    let t = Fields.get group_k r in
    Fields.add group_k { t with gid } r

  let users r = (Fields.get group_k r).users
  let set_users users r =
    let t = Fields.get group_k r in
    Fields.add group_k { t with users } r

  let empty = {
    name = "";
    passwd = "";
    gid = 0;
    users = [];
  }

  let create ~name ~passwd ~gid ~users r =
    Fields.add group_k
      { name; passwd; gid; users } r
end

module Stat = struct
  type label += Stat
  let _ = add_label_string Stat "stat"
  
  type mode = {
    rusr: bool; wusr: bool; xusr: bool;
    rgrp: bool; wgrp: bool; xgrp: bool;
    roth: bool; woth: bool; xoth: bool;
    suid: bool; sgid: bool; sticky: bool;
    bits: int;
  }

  type t = {
    dev: int;
    inode: int;
    kind: Unix.file_kind;
    mode: mode;
    nlink: int;
    uid: int;
    gid: int;
    rdev: int;
    size: int;
    blksize: int;
    blocks: int;
    atime: float;
    mtime: float;
    ctime: float
  }

  let stat_k = Fields.Key.create Stat

  module Mode = struct
    let xusr r = (Fields.get stat_k r).mode.xusr
    let set_xusr xusr r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with xusr }} r

    let wusr r = (Fields.get stat_k r).mode.wusr
    let set_wusr wusr r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with wusr }} r

    let rusr r = (Fields.get stat_k r).mode.rusr
    let set_rusr rusr r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with rusr }} r

    let xgrp r = (Fields.get stat_k r).mode.xgrp
    let set_xgrp xgrp r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with xgrp }} r

    let wgrp r = (Fields.get stat_k r).mode.wgrp
    let set_wgrp wgrp r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with wgrp }} r

    let rgrp r = (Fields.get stat_k r).mode.rgrp
    let set_rgrp rgrp r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with rgrp }} r

    let xoth r = (Fields.get stat_k r).mode.xoth
    let set_xoth xoth r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with xoth }} r

    let woth r = (Fields.get stat_k r).mode.woth
    let set_woth woth r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with woth }} r

    let roth r = (Fields.get stat_k r).mode.roth
    let set_roth roth r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with roth }} r

    let suid r = (Fields.get stat_k r).mode.suid
    let set_suid suid r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with suid }} r

    let sgid r = (Fields.get stat_k r).mode.sgid
    let set_sgid sgid r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with sgid }} r

    let sticky r = (Fields.get stat_k r).mode.sticky
    let set_sticky sticky r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with sticky }} r

    let bits r = (Fields.get stat_k r).mode.bits
    let set_bits bits r =
      let t = Fields.get stat_k r in
      Fields.add stat_k { t with mode = { t.mode with bits }} r

    let empty = {
      xusr = false;
      wusr = false;
      rusr = false;
      xgrp = false;
      wgrp = false;
      rgrp = false;
      xoth = false;
      woth = false;
      roth = false;
      suid = false;
      sgid = false;
      sticky = false;
      bits = 0;
    }

    let create
        ~xusr ~wusr ~rusr
        ~xgrp ~wgrp ~rgrp
        ~xoth ~woth ~roth
        ~suid ~sgid ~sticky
        ~bits r =
      let t = Fields.get stat_k r in
      Fields.add stat_k
        { t with mode = {
            xusr; wusr; rusr;
            xgrp; wgrp; rgrp;
            xoth; woth; roth;
            suid; sgid; sticky;
            bits }} r
  end

  let dev r = (Fields.get stat_k r).dev
  let set_dev dev r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with dev } r

  let inode r = (Fields.get stat_k r).inode
  let set_inode inode r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with inode } r

  let kind r = (Fields.get stat_k r).kind
  let set_kind kind r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with kind } r

  let nlink r = (Fields.get stat_k r).nlink
  let set_nlink nlink r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with nlink } r

  let uid r = (Fields.get stat_k r).uid
  let set_uid uid r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with uid } r

  let gid r = (Fields.get stat_k r).gid
  let set_gid gid r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with gid } r
  
  let rdev r = (Fields.get stat_k r).rdev
  let set_rdev rdev r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with rdev } r

  let size r = (Fields.get stat_k r).size
  let set_size size r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with size } r

  let blksize r = (Fields.get stat_k r).blksize
  let set_blksize blksize r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with blksize } r

  let blocks r = (Fields.get stat_k r).blocks
  let set_blocks blocks r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with blocks } r

  let atime r = (Fields.get stat_k r).atime
  let set_atime atime r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with atime } r

  let mtime r = (Fields.get stat_k r).mtime
  let set_mtime mtime r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with mtime } r

  let ctime r = (Fields.get stat_k r).ctime
  let set_ctime ctime r =
    let t = Fields.get stat_k r in
    Fields.add stat_k { t with ctime } r

  let empty = {
    dev = 0; inode = 0;
    kind = Unix.S_REG;
    mode = Mode.empty;
    nlink = 0;
    uid = 0; gid = 0;
    rdev = 0;
    size = 0; blksize = 0; blocks = 0;
    atime = 0.0; mtime = 0.0; ctime = 0.0;
  }

  let create
      ~dev ~inode ~kind ~nlink ~uid ~gid
      ~rdev ~size ~atime ~mtime ~ctime r =
    Fields.add stat_k
      { dev; inode; kind; mode = Mode.empty;
        nlink; uid; gid; rdev; size;
        blksize = 0; blocks = 0;
        atime; mtime; ctime } r
end

module Ps = struct
  type t = {
    user: string;
    pid: int;
    pcpu: float;
    pmem: float;
    vsz: int;
    rss: int;
    tt: string;
    stat: string;
    started: string;
    time: string;
    command: string
  }

  type label += Ps
  let _ = add_label_string Ps "ps"

  let ps_k = Fields.Key.create Ps

  let user r = (Fields.get ps_k r).user
  let set_user user r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with user } r

  let pid r = (Fields.get ps_k r).pid
  let set_pid pid r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with pid } r

  let pcpu r = (Fields.get ps_k r).pcpu
  let set_pcpu pcpu r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with pcpu } r

  let pmem r = (Fields.get ps_k r).pmem
  let set_pmem pmem r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with pmem } r

  let vsz r = (Fields.get ps_k r).vsz
  let set_vsz vsz r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with vsz } r

  let rss r = (Fields.get ps_k r).rss
  let set_rss rss r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with rss } r

  let tt r = (Fields.get ps_k r).tt
  let set_tt tt r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with tt } r

  let stat r = (Fields.get ps_k r).stat
  let set_stat stat r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with stat } r

  let started r = (Fields.get ps_k r).started
  let set_started started r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with started } r

  let time r = (Fields.get ps_k r).time
  let set_time time r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with time } r

  let command r = (Fields.get ps_k r).command
  let set_command command r =
    let t = Fields.get ps_k r in
    Fields.add ps_k { t with command } r

  let empty = {
    user = "";
    pid = 0;
    pcpu = 0.0; pmem = 0.0;
    vsz = 0; rss = 0;
    tt = ""; stat = "";
    started = "";
    time = "";
    command = "";
  }

  let create
      ~user ~pid ~pcpu ~pmem ~vsz ~rss
      ~tt ~stat ~started ~time ~command r =
    Fields.add ps_k
      { user; pid; pcpu; pmem; vsz; rss;
        tt; stat; started; time; command } r
end

module Fstab = struct
  type t = {
    file_system: string;
    mount_point: string;
    fstype: string;
    options: string list;
    dump: int;
    pass: int;
  }

  type label += Fstab
  let _ = add_label_string Fstab "fstab"
  
  let fstab_k = Fields.Key.create Fstab
  
  let file_system r = (Fields.get fstab_k r).file_system
  let set_file_system file_system r =
    let t = Fields.get fstab_k r in
    Fields.add fstab_k { t with file_system } r
  
  let mount_point r = (Fields.get fstab_k r).mount_point
  let set_mount_point mount_point r =
    let t = Fields.get fstab_k r in
    Fields.add fstab_k { t with mount_point } r

  let fstype r = (Fields.get fstab_k r).fstype
  let set_fstype fstype r =
    let t = Fields.get fstab_k r in
    Fields.add fstab_k { t with fstype } r

  let options r = (Fields.get fstab_k r).options
  let set_options options r =
    let t = Fields.get fstab_k r in
    Fields.add fstab_k { t with options } r

  let dump r = (Fields.get fstab_k r).dump
  let set_dump dump r =
    let t = Fields.get fstab_k r in
    Fields.add fstab_k { t with dump } r

  let pass r = (Fields.get fstab_k r).pass
  let set_pass pass r =
    let t = Fields.get fstab_k r in
    Fields.add fstab_k { t with pass } r

  let empty = {
    file_system = "";
    mount_point = "";
    fstype = "";
    options = [];
    dump = 0;
    pass = 0;
  }

  let create
      ~file_system ~mount_point ~fstype
      ~options ~dump ~pass r =
    Fields.add fstab_k
      { file_system; mount_point; fstype;
        options; dump; pass } r
end

module Mailcap = struct
  type t = {
    content_type: string;
    command: string;
    flags: string list;
    fields: (string * string) list
  }

  type label += Mailcap
  let _ = add_label_string Mailcap "mailcap"

  let mailcap_k = Fields.Key.create Mailcap

  let content_type r = (Fields.get mailcap_k r).content_type
  let set_content_type content_type r =
    let t = Fields.get mailcap_k r in
    Fields.add mailcap_k { t with content_type } r

  let command r = (Fields.get mailcap_k r).command
  let set_command command r =
    let t = Fields.get mailcap_k r in
    Fields.add mailcap_k { t with command } r

  let flags r = (Fields.get mailcap_k r).flags
  let set_flags flags r =
    let t = Fields.get mailcap_k r in
    Fields.add mailcap_k { t with flags } r

  let fields r = (Fields.get mailcap_k r).fields
  let set_fields fields r =
    let t = Fields.get mailcap_k r in
    Fields.add mailcap_k { t with fields } r

  let empty = {
    content_type = "";
    command = "";
    flags = [];
    fields = [];
  }

  let create ~content_type ~command ~flags ~fields r =
    Fields.add mailcap_k
      { content_type; command; flags; fields } r
end

let line ?(after = "\n") ?(before = "") raw =
  Fields.empty
  |> Fields.add raw_k raw
  |> Fields.add show_k (fun _ -> raw)
  |> Fields.add source_k `None
  |> Fields.add seq_k 0
  |> Fields.add after_k after
  |> Fields.add before_k before

let pp fmt line =
  Format.fprintf fmt "<line:\"%s\">" (String.escaped (show line))
