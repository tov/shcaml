open Util

type descr            = Unix.file_descr

type any_channel      = [ `InChannel  of in_channel
                        | `OutChannel of out_channel ]

type gen_in_channel   = [ `InChannel  of in_channel
                        | `InDescr    of descr
                        | `InFd       of int ]

type gen_out_channel  = [ `OutChannel of out_channel
                        | `OutDescr   of descr
                        | `OutFd      of int ]

type gen_channel      = [ gen_in_channel
                        | gen_out_channel ]

type dup_in_source    = [ gen_in_channel
                        | `Filename   of string
                        | `Close
                        | `Null ]

type dup_out_source   = [ gen_out_channel
                        | `Filename   of string
                        | `Close
                        | `Null
                        | `Filespec   of string * clobber_spec ]
and clobber_spec      = [ `Clobber | `NoClobber | `Append | `AppendOnly ]

type dup_source       = [ dup_in_source   | dup_out_source ]

type dup_in_spec      = (dup_in_source * gen_in_channel) list
type dup_out_spec     = (dup_out_source * gen_out_channel) list
type dup_spec         = (dup_source * gen_channel) list

type pipe_spec        = gen_channel list

type procref          = Proc.t option ref

let clobber = ref `Clobber

external descr_of_fd : int -> descr = "%identity"
external fd_of_descr : descr -> int = "%identity"

let descr_of_ic = Unix.descr_of_in_channel
let descr_of_oc = Unix.descr_of_out_channel

let descr_of_gen = function
| `InDescr d | `OutDescr d -> d
| `InFd n    | `OutFd n    -> descr_of_fd n
| `InChannel c             -> descr_of_ic c
| `OutChannel c            -> descr_of_oc c

module InDisposal = Disposal.Make (struct
  type t      = in_channel
  let equal   = (==)
  let hash c  = try Hashtbl.hash (descr_of_gen (`InChannel c))
                  with Sys_error("Bad file descriptor") -> 103
  let default = Pervasives.close_in
end)
 
module OutDisposal = Disposal.Make (struct
  type t      = out_channel
  let equal   = (==)
  let hash c  = try Hashtbl.hash (descr_of_gen (`OutChannel c))
                  with Sys_error("Bad file descriptor") -> 103
  let default = Pervasives.close_out
end)

let null_in ()  = Pervasives.open_in  "/dev/null"
let null_out () = Pervasives.open_out "/dev/null"

let close_in  = InDisposal.dispose
let close_out = OutDisposal.dispose

let open_file_in  = InDisposal.manage  % Pervasives.open_in
let open_file_out = OutDisposal.manage % Pervasives.open_out

let close_gen = function
| `InChannel c  -> close_in c
| `OutChannel c -> close_out c
| gen           -> Unix.close (descr_of_gen gen)

let dup_in = function
  | `Filename n  -> open_file_in n
  | `Close       -> let c = open_file_in "/dev/null" in
                      close_in c; c
  | `Null        -> null_in ()
  | #gen_in_channel as g
                 -> InDisposal.manage ^$
                      Unix.in_channel_of_descr ^$
                        Unix.dup ^$ descr_of_gen g

let get_flags = function
  | `Clobber    -> [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
  | `NoClobber  -> [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_EXCL]
  | `Append     -> [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND]
  | `AppendOnly -> [Unix.O_WRONLY;               Unix.O_APPEND]

let rec dup_out = function
  | `Filename s -> dup_out (`Filespec (s, !clobber))
  | `Filespec (s, spec)
                -> let d = Unix.openfile s (get_flags spec) 0o666 in
                     OutDisposal.manage (Unix.out_channel_of_descr d)
  | `Null       -> null_out ()
  | `Close      -> let c = open_file_out "/dev/null" in
                     close_out c; c
  | #gen_channel as g
                -> OutDisposal.manage ^$
                     Unix.out_channel_of_descr ^$
                       Unix.dup ^$ descr_of_gen g

let dup2 =
  let flush_in c =
    let d = descr_of_ic c in
      Unix.close d;
      try while true do ignore (input_char c) done
        with Sys_error _ -> () in
  let rec dup_any dir source dest =
    match dir, source with
    | `In,  (#gen_in_channel as gen)
    | `Out, (#gen_out_channel as gen)
                   -> Unix.dup2 (descr_of_gen gen) dest
    | _,    `Close -> Unix.close dest
    | `In,  (#dup_in_source as ds)
                   -> let ic    = dup_in ds in
                      let descr = descr_of_ic ic in
                      Unix.dup2 descr dest;
                      if dest <> descr then close_in ic
    | `Out, (#dup_out_source as ds)
                   -> let oc    = dup_out ds in
                      let descr = descr_of_oc oc in
                      Unix.dup2 descr dest;
                      if dest <> descr then close_out oc
    | _            -> raise (Invalid_argument "dup2") in
  fun (source, dest) -> match dest with
  | `OutChannel c -> flush c;
                     dup_any `Out source (descr_of_oc c)
  | `InChannel c  -> flush_in c;
                     dup_any `In source (descr_of_ic c)
  | `OutFd n      -> dup_any `Out source (descr_of_fd n)
  | `InFd n       -> dup_any `In source (descr_of_fd n)
  | `OutDescr d   -> dup_any `Out source d
  | `InDescr d    -> dup_any `In source d

let mov2 (source, dest) =
  match source with
  | #gen_channel as source
      when descr_of_gen dest <> descr_of_gen source
        -> dup2 ((source :> dup_source), dest);
           close_gen source
  | _   -> dup2 (source, dest)

let with_dup (source, dest) thunk =
  let destd = descr_of_gen dest in
  let saved = option_of_exn {| Unix.dup destd |} in
  begin try
    maybe saved ignore Unix.set_close_on_exec;
    dup2 (source, dest)
  with e -> maybe saved ignore Unix.close; raise e end;
  unwind_protect thunk {|
    match saved, dest with
    | None  , _                -> close_gen dest
    | Some d, #gen_in_channel  -> mov2 (`InDescr d, dest)
    | Some d, #gen_out_channel -> mov2 (`OutDescr d, dest)
  |}

let rec with_dups dups thunk = match dups with
  | []    -> thunk ()
  | x::xs -> with_dup x {| with_dups xs thunk |}

let rec connect_child =
  let connect_in fd (ifd, ofd) =
    Unix.dup2 ifd fd;
    Unix.close ifd;
    Unix.close ofd in
  let connect_out fd (ifd, ofd) =
    Unix.dup2 ofd fd;
    Unix.close ifd;
    Unix.close ofd in
  function
  | `InDescr fd   -> connect_in fd
  | `InFd n       -> connect_in (descr_of_fd n)
  | `InChannel c  -> connect_in (descr_of_ic c)
  | `OutDescr fd  -> connect_out fd
  | `OutFd n      -> connect_out (descr_of_fd n)
  | `OutChannel c -> connect_out (descr_of_oc c)

let connect_parent = function
| #gen_in_channel  -> fun (ifd, ofd) ->
    Unix.close ifd;
    `OutChannel (OutDisposal.manage (Unix.out_channel_of_descr ofd))
| #gen_out_channel -> fun (ifd, ofd) ->
    Unix.close ofd;
    `InChannel (InDisposal.manage (Unix.in_channel_of_descr ifd))

let open_thunk ?(pipes=[]) ?(dups=[]) thunk =
  let ceci_n'est_pas_une_liste_des_pipes =
      List.map (Unix.pipe % ignore) pipes in
  let proc = Proc.spawn {|
        List.iter2 connect_child pipes ceci_n'est_pas_une_liste_des_pipes;
        List.iter dup2 dups;
        thunk ()
      |} in
  proc, List.map2 connect_parent pipes ceci_n'est_pas_une_liste_des_pipes

let stash pid = function
  | None    -> ()
  | Some pr -> pr := Some pid

let open_thunk_in ?procref ?dups thunk =
  match open_thunk ~pipes:[ `OutDescr Unix.stdout ] ?dups thunk with
  | pid, [ `InChannel c ] -> stash pid procref;
                             c
  | _ -> raise Bug

let open_thunk_out ?procref ?dups thunk =
  match open_thunk ~pipes:[ `InDescr Unix.stdin ] ?dups thunk with
  | pid, [ `OutChannel c ] -> stash pid procref;
                              c
  | _ -> raise Bug

let open_thunk2 ?procref ?dups thunk =
  match open_thunk ~pipes:[ `OutDescr Unix.stdout;
                            `OutDescr Unix.stderr ]
                    ?dups thunk with
  | pid, [ `InChannel out;
           `InChannel err ] -> stash pid procref;
                               out, err
  | _ -> raise Bug

let open_thunk3 ?procref ?dups thunk =
  match open_thunk ~pipes:[ `InDescr Unix.stdin;
                            `OutDescr Unix.stdout;
                            `OutDescr Unix.stderr ]
                   ?dups thunk with
  | pid, [ `OutChannel inc;
           `InChannel out;
           `InChannel err ] -> stash pid procref;
                               inc, out, err
  | _ -> raise Bug

let protect_thunk kont thunk =
  IVar.with_interprocess_protect ^$ fun protect ->
    kont {| protect thunk |}

let with_command kont command =
  protect_thunk kont {| Proc.exec command |}

let open_command ?pipes ?dups =
  with_command (open_thunk ?pipes ?dups)

let open_command_in ?procref ?dups =
  with_command (open_thunk_in ?procref ?dups)

let open_command_out ?procref ?dups =
  with_command (open_thunk_out ?procref ?dups)

let open_command2 ?procref ?dups =
  with_command (open_thunk2 ?procref ?dups)

let open_command3 ?procref ?dups =
  with_command (open_thunk3 ?procref ?dups)

let with_program kont ?path prog ?argv0 args =
  protect_thunk kont {| Proc.exec_program ?path prog ?argv0 args |}

let open_program ?pipes ?dups =
  with_program (open_thunk ?pipes ?dups)

let open_program_in ?procref ?dups =
  with_program (open_thunk_in ?procref ?dups)

let open_program_out ?procref ?dups =
  with_program (open_thunk_out ?procref ?dups)

let open_program2 ?procref ?dups =
  with_program (open_thunk2 ?procref ?dups)

let open_program3 ?procref ?dups =
  with_program (open_thunk3 ?procref ?dups)

let open_string_in str =
  open_thunk_in {| print_string str |}

let string_of_channel c =
  let bufsize = 1024 in
  let buf     = Buffer.create bufsize in
  let str     = String.make bufsize '\000' in
  let rec loop c =
    match Pervasives.input c str 0 bufsize with
    | 0 -> Buffer.contents buf
    | n -> Buffer.add_substring buf str 0 n;
           loop c in
  loop c

let string_of_command ?procref cmd =
  let c = open_command_in ?procref cmd in
    unwind_protect {| string_of_channel c |} {| close_in c |}

let string_of_program ?procref ?path prog ?argv0 args =
  let c = open_program_in ?procref ?path prog ?argv0 args in
    unwind_protect {| string_of_channel c |} {| close_in c |}

let with_out_string kont =
  let subin, subout, suberr = open_thunk3 {|
    print_string (string_of_channel stdin)
  |} in
  unwind_protect {|
    let r = kont subin in
      close_out subin;
      r, string_of_channel subout
  |} {|
    close_out subin;
    close_in subout;
    close_in suberr
  |}

module Dup = struct
  type 'a dup_in_arg  = 'a
    constraint 'a = [> dup_in_source ] * [> gen_in_channel ]
  type 'a dup_out_arg = 'a
    constraint 'a = [> dup_out_source ] * [> gen_out_channel ]

  let ( !% ) d = fd_of_descr d

  let ( *<& )  a b = ((b, a) :> 'a dup_in_arg)
  let ( *>& )  a b = ((b, a) :> 'a dup_out_arg)
  let ( *< )   a b = a *<& `Filename b
  let ( *> )   a b = a *>& `Filename b
  let ( *>! )  a b = a *>& `Filespec (b, `Clobber)
  let ( *>? )  a b = a *>& `Filespec (b, `NoClobber)
  let ( *>> )  a b = a *>& `Filespec (b, `Append)
  let ( *>>! ) a b = a *>& `Filespec (b, `AppendOnly)

  let ( %<& )  a b = `InFd  a *<&  `InFd b
  let ( %>& )  a b = `OutFd a *>&  `OutFd b
  let ( %< )   a b = `InFd  a *<   b
  let ( %> )   a b = `OutFd a *>   b
  let ( %>! )  a b = `OutFd a *>!  b
  let ( %>? )  a b = `OutFd a *>?  b
  let ( %>> )  a b = `OutFd a *>>  b
  let ( %>>! ) a b = `OutFd a *>>! b

  let ( /<& )  a b = `InChannel  a *<&  `InChannel b
  let ( />& )  a b = `OutChannel a *>&  `OutChannel b
  let ( /< )   a b = `InChannel  a *<   b
  let ( /> )   a b = `OutChannel a *>   b
  let ( />! )  a b = `OutChannel a *>!  b
  let ( />? )  a b = `OutChannel a *>?  b
  let ( />> )  a b = `OutChannel a *>>  b
  let ( />>! ) a b = `OutChannel a *>>! b

  let ( *>% )  a b =             a *>& `OutFd      b
  let ( *>/ )  a b =             a *>& `OutChannel b
  let ( %>* )  a b = `OutFd      a *>& b
  let ( %>/ )  a b = `OutFd      a *>& `OutChannel b
  let ( />* )  a b = `OutChannel a *>& b
  let ( />% )  a b = `OutChannel a *>& `OutFd      b

  let ( *<% )  a b =            a *<& `InFd      b
  let ( *</ )  a b =            a *<& `InChannel b
  let ( %<* )  a b = `InFd      a *<& b
  let ( %</ )  a b = `InFd      a *<& `InChannel b
  let ( /<* )  a b = `InChannel a *<& b
  let ( /<% )  a b = `InChannel a *<& `InFd      b
end

type directory = {
  hash:   int;
  handle: Unix.dir_handle;
}

module DirDisposal = Disposal.Make (struct
  type t      = directory
  let equal   = (==)
  let hash d  = d.hash
  let default = ignore
end)

let low_close d = Unix.closedir d.handle

let opendir name =
  let hash   = Hashtbl.hash name in
  let handle = Unix.opendir name in
  DirDisposal.manage ~disposer:low_close {
    hash   = hash;
    handle = handle;
  }

let closedir    = DirDisposal.dispose
let readdir d   = Unix.readdir d.handle
let rewinddir d = Unix.rewinddir d.handle

