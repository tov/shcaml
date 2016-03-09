type label = ..

let label_strings = ref []

let string_of_label lbl =
  try List.assoc lbl !label_strings with
    Not_found -> raise (Failure "string_of_label")

let add_label_string lbl str =
  label_strings := (lbl, str) :: !label_strings

module Map = Mixmap.Make (struct
    type t = label
    let compare = compare
  end)

type t = Map.t

type source = [
  | `File of string
  | `Command of string
  | `Process of Proc.execspec
  | `Directory of string
  | `Other of string
  | `None
]

module Field = struct
  exception Not_found of label

  let _ =
    Printexc.register_printer (function
      | Not_found lbl -> Some (string_of_label lbl)
      | _ -> None)
  
  let get ~inj m lbl =
    match Map.get ~inj m lbl with
    | None -> raise (Not_found lbl)
    | Some v -> v

  let get_opt ~inj m name = Map.get ~inj m name

  let set ~inj name value m =
    Map.add ~inj m name value

  let string = Mixmap.create_inj ()
  let int = Mixmap.create_inj ()
  let string_array = Mixmap.create_inj ()
  let string_list = Mixmap.create_inj ()
  let source = Mixmap.create_inj ()
end

type label += Raw | Show | Source | Seq | After | Before

let _ =
  add_label_string Raw "Raw";
  add_label_string Show "Show";
  add_label_string Source "Source";
  add_label_string Seq "Seq";
  add_label_string After "After";
  add_label_string Before "Before"

let raw r = Field.get ~inj:Field.string r Raw
let set_raw = Field.set ~inj:Field.string Raw

let field_string_thunk : (unit -> string) Mixmap.injection =
  Mixmap.create_inj ()

let show r = (Field.get ~inj:field_string_thunk r Show) ()
let set_show s = Field.set ~inj:field_string_thunk Show (fun _ -> s)

let select sel r = Field.set ~inj:field_string_thunk Show (fun _ -> sel r) r

let source r = Field.get ~inj:Field.source r Source
let set_source = Field.set ~inj:Field.source Source

let seq r = Field.get ~inj:Field.int r Seq
let set_seq = Field.set ~inj:Field.int Seq

let after r = Field.get ~inj:Field.string r After
let set_after = Field.set ~inj:Field.string After

let before r = Field.get ~inj:Field.string r Before
let set_before = Field.set ~inj:Field.string Before

module Key_value = struct
  type t = { key : string; value : string; section: string }

  type label += Key_value
  let _ = add_label_string Key_value "Key_value"

  let inj : t Mixmap.injection = Mixmap.create_inj ()

  let key r = (Field.get ~inj r Key_value).key
  let set_key key r =
    let t = Field.get ~inj r Key_value in
    Field.set ~inj Key_value { t with key } r

  let value r = (Field.get ~inj r Key_value).value
  let set_value value r =
    let t = Field.get ~inj r Key_value in
    Field.set ~inj Key_value { t with value } r

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

  let section r = (Field.get ~inj r Key_value).section
  let set_section section r =
    let t = Field.get ~inj r Key_value in
    Field.set ~inj Key_value { t with section } r

  let empty = { key = ""; value = ""; section = "" }

  let create ~key ~value r =
    Field.set ~inj Key_value
      { key; value; section = "" } r
end

module Delim = struct
  type t = { options: Delimited.options;
             names: string list;
             fields: string array }

  type label += Delim
  let _ = add_label_string Delim "Delim"

  let inj : t Mixmap.injection = Mixmap.create_inj ()

  let fields r = (Field.get ~inj r Delim).fields
  let set_fields fields r =
    let t = Field.get ~inj r Delim in
    Field.set ~inj Delim { t with fields } r

  let names r = (Field.get ~inj r Delim).names
  let set_names names r =
    let t = Field.get ~inj r Delim in
    Field.set ~inj Delim { t with names } r

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

  let options r = (Field.get ~inj r Delim).options
  let set_options options r =
    let t = Field.get ~inj r Delim in
    Field.set ~inj Delim { t with options } r

  let output channel line =
    Delimited.output_record channel ~options: (options line) (fields line)

  let empty = {
    fields = [| |];
    names = [];
    options = Delimited.default_options
  }

  let create ~fields r =
    Field.set ~inj Delim
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
  let _ = add_label_string Passwd "Passwd"

  let inj : t Mixmap.injection = Mixmap.create_inj ()

  let name r = (Field.get ~inj r Passwd).name
  let set_name name r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with name } r

  let passwd r = (Field.get ~inj r Passwd).passwd
  let set_passwd passwd r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with passwd } r
  
  let uid r = (Field.get ~inj r Passwd).uid
  let set_uid uid r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with uid } r

  let gid r = (Field.get ~inj r Passwd).gid
  let set_gid gid r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with gid } r

  let gecos r = (Field.get ~inj r Passwd).gecos
  let set_gecos gecos r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with gecos } r

  let home r = (Field.get ~inj r Passwd).home
  let set_home home r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with home } r

  let shell r = (Field.get ~inj r Passwd).shell
  let set_shell shell r =
    let t = Field.get ~inj r Passwd in
    Field.set ~inj Passwd { t with shell } r

  let empty = {
    name = "";
    passwd = "";
    uid = 0; gid = 0;
    gecos = "";
    home = "";
    shell = "";
  }

  let create ~name ~passwd ~uid ~gid ~gecos ~home ~shell r =
    Field.set ~inj Passwd
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
  let _ = add_label_string Group "Group"
  
  let inj : t Mixmap.injection = Mixmap.create_inj ()

  let name r = (Field.get ~inj r Group).name
  let set_name name r =
    let t = Field.get ~inj r Group in
    Field.set ~inj Group { t with name } r

  let passwd r = (Field.get ~inj r Group).passwd
  let set_passwd passwd r =
    let t = Field.get ~inj r Group in
    Field.set ~inj Group { t with passwd } r

  let gid r = (Field.get ~inj r Group).gid
  let set_gid gid r =
    let t = Field.get ~inj r Group in
    Field.set ~inj Group { t with gid } r

  let users r = (Field.get ~inj r Group).users
  let set_users users r =
    let t = Field.get ~inj r Group in
    Field.set ~inj Group { t with users } r

  let empty = {
    name = "";
    passwd = "";
    gid = 0;
    users = [];
  }

  let create ~name ~passwd ~gid ~users r =
    Field.set ~inj Group
      { name; passwd; gid; users } r
end

module Stat = struct
  type label += Stat
  let _ = add_label_string Stat "Stat"
  
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

  let inj : t Mixmap.injection = Mixmap.create_inj ()

  module Mode = struct
    let xusr r = (Field.get ~inj r Stat).mode.xusr
    let set_xusr xusr r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with xusr }} r

    let wusr r = (Field.get ~inj r Stat).mode.wusr
    let set_wusr wusr r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with wusr }} r

    let rusr r = (Field.get ~inj r Stat).mode.rusr
    let set_rusr rusr r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with rusr }} r

    let xgrp r = (Field.get ~inj r Stat).mode.xgrp
    let set_xgrp xgrp r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with xgrp }} r

    let wgrp r = (Field.get ~inj r Stat).mode.wgrp
    let set_wgrp wgrp r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with wgrp }} r

    let rgrp r = (Field.get ~inj r Stat).mode.rgrp
    let set_rgrp rgrp r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with rgrp }} r

    let xoth r = (Field.get ~inj r Stat).mode.xoth
    let set_xoth xoth r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with xoth }} r

    let woth r = (Field.get ~inj r Stat).mode.woth
    let set_woth woth r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with woth }} r

    let roth r = (Field.get ~inj r Stat).mode.roth
    let set_roth roth r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with roth }} r

    let suid r = (Field.get ~inj r Stat).mode.suid
    let set_suid suid r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with suid }} r

    let sgid r = (Field.get ~inj r Stat).mode.sgid
    let set_sgid sgid r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with sgid }} r

    let sticky r = (Field.get ~inj r Stat).mode.sticky
    let set_sticky sticky r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with sticky }} r

    let bits r = (Field.get ~inj r Stat).mode.bits
    let set_bits bits r =
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat { t with mode = { t.mode with bits }} r

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
      let t = Field.get ~inj r Stat in
      Field.set ~inj Stat
        { t with mode = {
            xusr; wusr; rusr;
            xgrp; wgrp; rgrp;
            xoth; woth; roth;
            suid; sgid; sticky;
            bits }} r
  end

  let dev r = (Field.get ~inj r Stat).dev
  let set_dev dev r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with dev } r

  let inode r = (Field.get ~inj r Stat).inode
  let set_inode inode r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with inode } r

  let kind r = (Field.get ~inj r Stat).kind
  let set_kind kind r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with kind } r

  let nlink r = (Field.get ~inj r Stat).nlink
  let set_nlink nlink r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with nlink } r

  let uid r = (Field.get ~inj r Stat).uid
  let set_uid uid r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with uid } r

  let gid r = (Field.get ~inj r Stat).gid
  let set_gid gid r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with gid } r
  
  let rdev r = (Field.get ~inj r Stat).rdev
  let set_rdev rdev r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with rdev } r

  let size r = (Field.get ~inj r Stat).size
  let set_size size r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with size } r

  let blksize r = (Field.get ~inj r Stat).blksize
  let set_blksize blksize r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with blksize } r

  let blocks r = (Field.get ~inj r Stat).blocks
  let set_blocks blocks r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with blocks } r

  let atime r = (Field.get ~inj r Stat).atime
  let set_atime atime r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with atime } r

  let mtime r = (Field.get ~inj r Stat).mtime
  let set_mtime mtime r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with mtime } r

  let ctime r = (Field.get ~inj r Stat).ctime
  let set_ctime ctime r =
    let t = Field.get ~inj r Stat in
    Field.set ~inj Stat { t with ctime } r

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
    Field.set ~inj Stat
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
  let _ = add_label_string Ps "Ps"

  let inj = Mixmap.create_inj ()

  let user r = (Field.get ~inj r Ps).user
  let set_user user r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with user } r

  let pid r = (Field.get ~inj r Ps).pid
  let set_pid pid r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with pid } r

  let pcpu r = (Field.get ~inj r Ps).pcpu
  let set_pcpu pcpu r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with pcpu } r

  let pmem r = (Field.get ~inj r Ps).pmem
  let set_pmem pmem r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with pmem } r

  let vsz r = (Field.get ~inj r Ps).vsz
  let set_vsz vsz r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with vsz } r

  let rss r = (Field.get ~inj r Ps).rss
  let set_rss rss r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with rss } r

  let tt r = (Field.get ~inj r Ps).tt
  let set_tt tt r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with tt } r

  let stat r = (Field.get ~inj r Ps).stat
  let set_stat stat r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with stat } r

  let started r = (Field.get ~inj r Ps).started
  let set_started started r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with started } r

  let time r = (Field.get ~inj r Ps).time
  let set_time time r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with time } r

  let command r = (Field.get ~inj r Ps).command
  let set_command command r =
    let t = Field.get ~inj r Ps in
    Field.set ~inj Ps { t with command } r

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
    Field.set ~inj Ps
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
  let _ = add_label_string Fstab "Fstab"
  
  let inj = Mixmap.create_inj ()
  
  let file_system r = (Field.get ~inj r Fstab).file_system
  let set_file_system file_system r =
    let t = Field.get ~inj r Fstab in
    Field.set ~inj Fstab { t with file_system } r
  
  let mount_point r = (Field.get ~inj r Fstab).mount_point
  let set_mount_point mount_point r =
    let t = Field.get ~inj r Fstab in
    Field.set ~inj Fstab { t with mount_point } r

  let fstype r = (Field.get ~inj r Fstab).fstype
  let set_fstype fstype r =
    let t = Field.get ~inj r Fstab in
    Field.set ~inj Fstab { t with fstype } r

  let options r = (Field.get ~inj r Fstab).options
  let set_options options r =
    let t = Field.get ~inj r Fstab in
    Field.set ~inj Fstab { t with options } r

  let dump r = (Field.get ~inj r Fstab).dump
  let set_dump dump r =
    let t = Field.get ~inj r Fstab in
    Field.set ~inj Fstab { t with dump } r

  let pass r = (Field.get ~inj r Fstab).pass
  let set_pass pass r =
    let t = Field.get ~inj r Fstab in
    Field.set ~inj Fstab { t with pass } r

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
    Field.set ~inj Fstab
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
  let _ = add_label_string Mailcap "Mailcap"

  let inj = Mixmap.create_inj ()

  let content_type r = (Field.get ~inj r Mailcap).content_type
  let set_content_type content_type r =
    let t = Field.get ~inj r Mailcap in
    Field.set ~inj Mailcap { t with content_type } r

  let command r = (Field.get ~inj r Mailcap).command
  let set_command command r =
    let t = Field.get ~inj r Mailcap in
    Field.set ~inj Mailcap { t with command } r

  let flags r = (Field.get ~inj r Mailcap).flags
  let set_flags flags r =
    let t = Field.get ~inj r Mailcap in
    Field.set ~inj Mailcap { t with flags } r

  let fields r = (Field.get ~inj r Mailcap).fields
  let set_fields fields r =
    let t = Field.get ~inj r Mailcap in
    Field.set ~inj Mailcap { t with fields } r

  let empty = {
    content_type = "";
    command = "";
    flags = [];
    fields = [];
  }

  let create ~content_type ~command ~flags ~fields r =
    Field.set ~inj Mailcap
      { content_type; command; flags; fields } r
end

let line ?(after = "\n") ?(before = "") raw =
  Map.empty
  |> Field.set ~inj:Field.string Raw raw
  |> Field.set ~inj:field_string_thunk Show (fun _ -> raw)
  |> Field.set ~inj:Field.source Source `None
  |> Field.set ~inj:Field.int Seq 0
  |> Field.set ~inj:Key_value.inj Key_value.Key_value Key_value.empty
  |> Field.set ~inj:Delim.inj Delim.Delim Delim.empty
  |> Field.set ~inj:Passwd.inj Passwd.Passwd Passwd.empty
  |> Field.set ~inj:Group.inj Group.Group Group.empty
  |> Field.set ~inj:Stat.inj Stat.Stat Stat.empty
  |> Field.set ~inj:Ps.inj Ps.Ps Ps.empty
  |> Field.set ~inj:Fstab.inj Fstab.Fstab Fstab.empty
  |> Field.set ~inj:Mailcap.inj Mailcap.Mailcap Mailcap.empty
  |> Field.set ~inj:Field.string After after
  |> Field.set ~inj:Field.string Before before

let pp fmt line =
  Format.fprintf fmt "<line:\"%s\">" (String.escaped (show line))
