open Util
open Pcre

module Convert = struct
  let convert convertor ty loc str =
    try convertor str with
    | Failure _ ->
        Shtream.warn "%s: %s expected, got `%s'" loc ty str

  let to_int   = convert int_of_string "int"
  let to_float = convert float_of_string "float"
end

type adaptor = Line.t Shtream.t -> Line.t Shtream.t

type splitter = Line.t -> Line.t

type fitting_adaptor = unit -> (Line.t -> Line.t) Fitting.t

let make_adaptor ?reader splitter =
  fun shtream ->
    maybe reader ignore (Shtream.hint_reader shtream);
    Shtream.map splitter shtream

let split_helper ?(max = 0) delim =
  Delimited.splitter ~options:{
    Delimited.default_options with
      Delimited.field_sep     = delim;
      Delimited.max_fields    = max;
      Delimited.rec_quotation = false;
      Delimited.trim_space    = false;
  }

module Delim = struct
  let reader ?options channel = Delimited.reader ?options channel

  let splitter ?options =
    let options = maybe options (fun _ -> Delimited.default_options) id in
    fun line ->
      let fields = Delimited.splitter ~options (Line.raw line) in
        Line.Delim.set_options options
          (Line.Delim.create ~fields line)

  let adaptor ?options =
    make_adaptor ~reader:(reader ?options) (splitter ?options)

  let fitting ?options () = Fitting.trans (adaptor ?options)

  module type SPEC = sig
    val options : Delimited.options
  end

  module type S = sig
    include SPEC
    val adaptor : adaptor
    val fitting : fitting_adaptor
    val reader : Reader.t
    val splitter : splitter
  end

  module Make(Spec : SPEC) = struct
    include Spec

    let splitter line   = splitter ~options line
    let reader          = reader ~options
    let adaptor shtream = make_adaptor ~reader splitter shtream
    let fitting ()      = Fitting.trans adaptor
  end

  module type SPEC_NAMES = sig
    val options : Delimited.options
    val names   : string list
  end

  module type S_NAMES = sig
    include SPEC_NAMES
    val adaptor : adaptor
    val fitting : fitting_adaptor
    val reader : Reader.t
    val splitter : splitter
  end

  module Make_names(Spec : SPEC_NAMES) = struct
    include Spec

    let splitter line   = Line.Delim.set_names names
                            (splitter ~options line)
    let reader          = reader ~options
    let adaptor shtream = make_adaptor ~reader splitter shtream
    let fitting ()      = Fitting.trans adaptor
  end
end

module SimpleFlatFile = struct
  let reader ?comments ?(blanks = true) = match comments, blanks with
    | Some pat, true ->
        Reader.ignore_if
          (fun str -> Reader.starts_with pat str || Reader.blank str)
          Reader.lines
    | Some pat, false ->
        Reader.ignore_if (Reader.starts_with pat) Reader.lines
    | None, true ->
        Reader.ignore_if Reader.blank Reader.lines
    | None, false ->
        Reader.lines

  let splitter ?(max = 0) delim =
    Delim.splitter ~options:{
      Delimited.default_options with
        Delimited.field_sep     = delim;
        Delimited.max_fields    = max;
        Delimited.rec_quotation = false;
        Delimited.trim_space    = false;
    }

  let adaptor ?comments ?blanks ?max delim =
    make_adaptor ~reader:(reader ?comments ?blanks)
                 (splitter ?max delim)

  let fitting ?comments ?blanks ?max delim () =
    Fitting.trans (adaptor ?comments ?blanks ?max delim)

  module type SPEC = sig
    val comments : string option
    val blanks   : bool
    val max      : int
    val delim    : char
  end

  module type S = sig
    include SPEC
    val adaptor : adaptor
    val fitting : fitting_adaptor
    val reader : Reader.t
    val splitter : splitter
  end

  module Make(Spec : SPEC) = struct
    include Spec

    let splitter line   = splitter ~max delim line
    let reader          = reader ?comments ~blanks
    let adaptor shtream = make_adaptor ~reader splitter shtream
    let fitting ()      = Fitting.trans adaptor
  end

  module type SPEC_NAMES = sig
    val comments : string option
    val blanks   : bool
    val max      : int
    val delim    : char
    val names    : string list
  end

  module type S_NAMES = sig
    include SPEC_NAMES
    val adaptor : adaptor
    val fitting : fitting_adaptor
    val reader : Reader.t
    val splitter : splitter
  end

  module Make_names(Spec : SPEC_NAMES) = struct
    include Spec

    let splitter line   = Line.Delim.set_names names
                            (splitter ~max delim line)
    let reader          = reader ?comments ~blanks
    let adaptor shtream = make_adaptor ~reader splitter shtream
    let fitting ()      = Fitting.trans adaptor
  end
end

module Passwd = struct
  let loc = "Passwd.splitter"

  let reader        = SimpleFlatFile.reader ~comments:"#" ~blanks:true

  let splitter line =
    match split_helper ~max:7 ':' (Line.raw line) with
    | [| name; passwd; uid; gid; gecos; home; shell; |] ->
        Line.Passwd.create ~name ~passwd ~gecos ~home ~shell
                           ~uid:(Convert.to_int loc uid)
                           ~gid:(Convert.to_int loc gid) line
    | a -> Shtream.warn
             "Passwd.splitter: passwd line has %d fields, needs 7"
             (Array.length a)

  let adaptor shtream = make_adaptor ~reader splitter shtream

  let fitting () = Fitting.trans adaptor
end

module Group = struct
  let loc = "Group.splitter"

  let reader        = SimpleFlatFile.reader ~comments:"#" ~blanks:true

  let split_users str =
    match split_helper ',' str with
    | [|""|] -> [ ]
    | lst    -> Array.to_list lst

  let splitter line =
    match split_helper ~max:4 ':' (Line.raw line) with
    | [| name; passwd; gid; users |] ->
        Line.Group.create ~name ~passwd
                          ~gid:(Convert.to_int loc gid)
                          ~users:(split_users users) line
    | a -> Shtream.warn
             "Group.splitter: group line has %d fields, needs 4"
             (Array.length a)

  let adaptor shtream = make_adaptor ~reader splitter shtream

  let fitting () = Fitting.trans adaptor
end

module Fstab = struct
  let loc = "Fstab.splitter"

  let reader = SimpleFlatFile.reader ~comments:"#" ~blanks:true

  let split_string =
    Delimited.splitter ~options:{ Delimited.default_options with
      Delimited.field_sep     = ' ';
      Delimited.rec_quotation = false;
    }

  let split_options = Group.split_users

  let splitter line =
    match split_string (Line.raw line), "0", "0" with
    | [| file_system; mount_point; fstype; options; dump; pass; |], _, _
    | [| file_system; mount_point; fstype; options; dump; |], _, pass
    | [| file_system; mount_point; fstype; options; |], dump, pass ->
        Line.Fstab.create ~file_system ~mount_point ~fstype
                          ~options:(split_options options)
                          ~dump:(Convert.to_int loc dump)
                          ~pass:(Convert.to_int loc pass) line
    | a, _, _ ->
        Shtream.warn
          "Fstab.splitter: fstab line has %d fields, needs 4-6"
          (Array.length a)

  let adaptor shtream = make_adaptor ~reader splitter shtream

  let fitting () = Fitting.trans adaptor
end

module Stat = struct
  let reader = Reader.lines

  open Unix

  let modeify bits =
    let is_set n = (bits land n) > 0 in
      Line.Stat.Mode.create 
        ~bits:   bits
        ~xoth:   (is_set 0o0001)
        ~woth:   (is_set 0o0002)
        ~roth:   (is_set 0o0004)
        ~xgrp:   (is_set 0o0010)
        ~wgrp:   (is_set 0o0020)
        ~rgrp:   (is_set 0o0040)
        ~xusr:   (is_set 0o0100)
        ~wusr:   (is_set 0o0200)
        ~rusr:   (is_set 0o0400)
        ~sticky: (is_set 0o1000)
        ~sgid:   (is_set 0o2000)
        ~suid:   (is_set 0o4000)

  let splitter ?dir file = 
    let filename = match dir with
      | Some d -> Filename.concat d (Line.raw file)
      | _      -> Line.raw file in
    let sb = Unix.stat filename in
      modeify sb.st_perm @@
        Line.Stat.create
          ~dev:   sb.st_dev
          ~inode: sb.st_ino
          ~kind:  sb.st_kind
          ~uid:   sb.st_uid
          ~gid:   sb.st_gid
          ~size:  sb.st_size
          ~nlink: sb.st_nlink
          ~rdev:  sb.st_rdev
          ~atime: sb.st_atime
          ~mtime: sb.st_mtime
          ~ctime: sb.st_ctime file

  let adaptor ?dir shtream =
    make_adaptor ~reader (splitter ?dir) shtream

  let fitting ?dir () = Fitting.trans (adaptor ?dir)
end

module Ps = struct
  let loc = "Ps.splitter"

  let reader = Reader.lines

  let split_string =
    Delimited.splitter ~options:{
      Delimited.default_options with
        Delimited.field_sep     = ' ';
        Delimited.max_fields    = 11;
        Delimited.rec_quotation = false;
    }

  let splitter ?(skip = true) line =
    if Line.seq line = 0 then Shtream.try_again ();
    match split_string (Line.raw line) with
    | [| user; pid; pcpu; pmem; vsz; rss;
         tt; stat; started; time; command; |] ->
        Line.Ps.create 
          ~user
          ~pcpu:(Convert.to_float loc pcpu)
          ~pmem:(Convert.to_float loc pmem)
          ~pid:(Convert.to_int loc pid)
          ~vsz:(Convert.to_int loc vsz)
          ~rss:(Convert.to_int loc rss)
          ~tt ~stat ~started ~time ~command
          line
    | a -> Shtream.warn
             "Ps.splitter: ps line has %d fields, needs 11"
             (Array.length a)

  let adaptor ?skip shtream =
    make_adaptor ~reader (splitter ?skip) shtream

  let fitting ?skip () = Fitting.trans (adaptor ?skip)
end

module Mailcap = struct
  let reader = Reader.join_on '\\'
                 (Reader.ignore_if
                   (fun s -> Reader.blank s ||
                             Reader.starts_with "#" s)
                   Reader.lines)

  (* Bug: This can't distinguish escaped % from unescaped %. *)
  let options = {
    Delimited.default_options with
      Delimited.rec_quotation = false;
      Delimited.rec_backslash = true;
      Delimited.field_sep     = '=';
      Delimited.max_fields    = 2;
  }

  let do_rest (flags, fields) field =
    match Delimited.splitter ~options field with
    | [| flag |] -> (flag :: flags, fields)
    | [| k; v |] -> (flags, (k, v) :: fields)
    | _          -> (flags, fields)

  let options = {
    Delimited.default_options with
      Delimited.rec_quotation = false;
      Delimited.rec_backslash = true;
      Delimited.field_sep     = ';';
  }

  let splitter line =
    match Array.to_list (Delimited.splitter ~options (Line.raw line)) with
    | [] | [_] ->
        Shtream.warn
          "Mailcap.splitter: mailcap line must have at least two fields"
    | content_type :: command :: rest ->
        let flags, fields = List.fold_left do_rest ([], []) rest in
          Line.Mailcap.create ~content_type ~command ~flags ~fields line

  let adaptor line = make_adaptor ~reader splitter line

  let fitting () = Fitting.trans adaptor
end

module Key_value = struct
  let reader ?(comment = "#") =
    Reader.join_on '\\'
      (Reader.ignore_if
        (fun s -> Reader.blank s ||
                  Reader.starts_with comment s)
        Reader.lines)

  let splitter ?(quiet = false) ?(delim = '=') line =
    let options = {
      Delimited.default_options with
        Delimited.rec_quotation = false;
        Delimited.field_sep     = delim;
        Delimited.max_fields    = 2; } in
    match Delimited.splitter ~options (Line.raw line) with
    | [| key; value |] -> Line.Key_value.create ~key ~value line
    | _ when quiet -> Shtream.try_again ()
    | a -> Shtream.warn
             "Key_value.splitter: key_value line has %d fields, needs 2"
             (Array.length a)

  let adaptor ?quiet ?comment ?delim shtream =
    make_adaptor ~reader:(reader ?comment) (splitter ?quiet ?delim) shtream

  let fitting ?quiet ?comment ?delim () =
    Fitting.trans (adaptor ?quiet ?comment ?delim)

  module type SPEC = sig
    val delim       : char
    val comment     : string
  end

  module type S = sig
    include SPEC
    val adaptor : adaptor
    val fitting : fitting_adaptor
    val reader : Reader.t
    val splitter : splitter
  end

  module Make(Spec : SPEC) : S = struct
    include Spec

    let splitter line   = splitter ~delim line
    let reader          = reader ~comment
    let adaptor shtream = make_adaptor ~reader splitter shtream
    let fitting ()      = Fitting.trans adaptor
  end
end

module Key_value_section = struct
  let reader ?(comment = "#") =
    Reader.join_on '\\'
      (Reader.ignore_if
        (fun s -> Reader.blank s ||
                  Reader.starts_with comment s)
        Reader.lines)

  let splitter ?(delim = '=') ?end_section section =
    let sec     = ref "" in
    let rex     = Pcre.regexp section in
    let rex_end = match end_section with
                  | None   -> None
                  | Some s -> Some (Pcre.regexp s) in
    let split   = Key_value.splitter ~delim in
      fun line ->
        begin try
          match Pcre.get_opt_substrings ~full_match:false
                  (Pcre.exec ~rex (Line.raw line)) with
          | [| Some s |] -> sec := s; Shtream.try_again ()
          | _            -> ()
        with Not_found -> () end;
        begin match rex_end with
          | Some rex ->
              if Pcre.pmatch ~rex (Line.raw line)
              then sec := ""; Shtream.try_again ()
          | _ -> ()
        end;
        Line.Key_value.set_section !sec (split line)

  let adaptor ?comment ?delim ?end_section section shtream =
    make_adaptor ~reader:(reader ?comment)
                 (splitter ?end_section ?delim section) shtream

  let fitting ?comment ?delim ?end_section section () =
    Fitting.trans (adaptor ?comment ?delim ?end_section section)

  module type SPEC = sig
    val section     : string
    val delim       : char
    val comment     : string
    val end_section : string option
  end

  module type S = sig
    include SPEC
    val adaptor : adaptor
    val fitting : fitting_adaptor
    val reader : Reader.t
    val new_splitter : unit -> splitter
  end

  module Make(Spec : SPEC) : S = struct
    include Spec

    let new_splitter ()  = splitter ?end_section ~delim section
    let reader           = reader ~comment
    let adaptor shtream  = make_adaptor ~reader (new_splitter ()) shtream
    let fitting ()       = Fitting.trans adaptor
  end
end

module Csv = struct
  let reader channel = Delim.reader channel

  let splitter ?(trim_space = true) =
    Delim.splitter ~options:{ Delimited.default_options with
      Delimited.trim_space = trim_space;
    }

  let adaptor ?trim_space shtream =
    make_adaptor ~reader (splitter ?trim_space) shtream

  let fitting ?trim_space () = Fitting.trans (adaptor ?trim_space)
end

module Ssh_config = Key_value_section.Make(struct
  let section     = "^\\s*Host (.+?)\\s*$"
  let delim       = ' '
  let comment     = "#"
  let end_section = None
end)

module Etc_aliases = Key_value.Make(struct
  let comment = "#"
  let delim   = ':'
end)

module Etc_hosts = Key_value.Make(struct
  let comment = "#"
  let delim   = ' '
end)

module Ini_file = Key_value_section.Make(struct
  let comment = ";"
  let delim   = '='
  let section = "\\[(.*)\\]"
  let end_section = None
end)
