open Util
open Fitting

type 'a shtream = 'a Shtream.t
type 'a line    = 'a Line.t
type 'a fitting = 'a Fitting.t
type absent     = Line.absent
type present    = Line.present

let cat s       = LineShtream.output s

let cut selector = trans $ Shtream.map (Line.select selector)

let set_stat ?dir =
  Line.set_source (`Directory (maybe dir {| "." |} id)) %
    Adaptor.Stat.splitter ?dir

let stat filename =
  set_stat ~dir:(Filename.dirname filename)
           (Line.line (Filename.basename filename))

let from_directory name =
  caml {|
    let dir = Channel.opendir name in
    from_shtream ^$
    Shtream.from_low
      ~close:{| Channel.closedir dir |}
      {| try Line.set_source (`Directory name)
               (Line.line (Channel.readdir dir)) with
         | End_of_file -> raise Shtream.Failure |}
  |}

let ls dir =
  from_directory dir -| Adaptor.Stat.fitting ~dir ()

let ps () = 
  from_null -| program "ps" ["auxww"] -| Adaptor.Ps.fitting ()


let isatty fd =
  try ignore (Unix.tcgetattr fd); true with
  | Unix.Unix_error _ -> false

module Test = struct
  let stat2 f1 f2 = (stat f1,stat f2)

  let z = (=) ""
  let n = (<>) ""

  let ef f1 f2 = 
    try
      let (st1,st2) = stat2 f1 f2 in
      let (dev,ino) = Line.Stat.dev,Line.Stat.inode in
        (dev st1) = (dev st2) && (ino st1) = (ino st2)
    with _ -> false

  let nt f1 f2 = 
    try 
      let (st1,st2) = stat2 f1 f2 in
        Line.Stat.mtime st1 > Line.Stat.mtime st2
    with _ -> false

  let ot f1 f2 = 
    try 
      let (st1,st2) = stat2 f1 f2 in
        Line.Stat.mtime st1 < Line.Stat.mtime st2
    with _ -> false


  let file_pred pred f = try pred (stat f) with _ -> false
  let file_kind k = file_pred (((=) k) % Line.Stat.kind)

  let b = file_kind Unix.S_BLK
  let c = file_kind Unix.S_CHR
  let d = file_kind Unix.S_DIR
  let f = file_kind Unix.S_REG
  let h = file_kind Unix.S_LNK (* broken; use lstat *)
  let p = file_kind Unix.S_FIFO

  let e = file_pred (const true)
  let g = file_pred Line.Stat.Mode.sgid
  let k = file_pred Line.Stat.Mode.sticky
  let s = file_pred ((<) 0 % Line.Stat.size)
  let u = file_pred Line.Stat.Mode.suid

  let r f = try Unix.access f [Unix.R_OK];true with _ -> false
  let w f = try Unix.access f [Unix.W_OK];true with _ -> false
  let x f = try Unix.access f [Unix.X_OK];true with _ -> false

  let t d = isatty (Channel.descr_of_fd d)

  let tfile f =
    r f && let c = Channel.open_file_in f in
             unwind_protect
               {| isatty (Unix.descr_of_in_channel c) |}
               {| Channel.close_in c |}

  (* problematic: 
   * -S (capitals in general) *)

  let rec test spec = 
    match spec with
        `Exists    -> e
      | `Char      -> c
      | `Dir       -> d
      | `Reg       -> f
      | `Link      -> h
      | `Pipe      -> p
      | `Sgid      -> s
      | `Sticky    -> k
      | `NonEmpty  -> s
      | `Suid      -> u
      | `Tty       -> tfile
      | `Read      -> r
      | `Write     -> w
      | `Execute   -> x
      | `Newer   f -> nt f
      | `Older   f -> ot f
      | `Equal   f -> ef f
      | `And (e1, e2) -> fun x -> (test e1 x) && (test e2 x)
      | `Or  (e1, e2) -> fun x -> (test e1 x) || (test e2 x)
end

let echo msg =
  trans {| Shtream.of_list [ Line.line msg ] |}

let cd    = Unix.chdir
let pwd   = Unix.getcwd
let sleep = Unix.sleep
let mkdir = flip Unix.mkdir 0o777

let mkpath =
  let rex        = Pcre.regexp "/" in
  let mkdir name =
    try mkdir name with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> () in
  fun path ->
    match Pcre.split ~rex path with
    | []  -> ()
    | lst ->
        let each rest next =
          match rest, next with
          | "", "" -> "/"
          | "", _  -> mkdir next; next
          | _,  _  -> let dir = rest^"/"^next in
                      mkdir dir; dir in
        ignore ^$ List.fold_left each "" lst

let backquote =
  String.concat " " %
    LineShtream.string_list_of %
      LineShtream.of_command

let lift_to_line2 f x y = f (Line.show x) (Line.show y)

let sort ?(compare = lift_to_line2 String.compare) () =
  trans $ Shtream.of_list % (List.fast_sort compare) % Shtream.list_of

let head n = trans
  (fun s ->
     Shtream.from ^$ fun i ->
       if i < n
       then Shtream.next' s
       else None)

let head_while pred = trans
  (fun shtr ->
     let each _ = 
       match Shtream.peek shtr with
         | Some v when pred v ->
             Shtream.next' shtr
         | _ -> None
     in Shtream.from each)

let behead_while pred = 
  trans
    (fun s -> 
       while maybe (Shtream.peek s) {| false |} pred do
         Shtream.junk s
       done; s)

let behead n = 
  let count = ref 0 in 
    behead_while 
      (fun s ->
         !count < n BEFORE (count := !count + 1))

let uniq ?(equal = lift_to_line2 (=)) () =
  trans $ fun shtr ->
    let memory = ref None in
    let pred line = 
      match !memory with
        | Some last when equal last line ->
            false
        | _ ->
            memory := Some line; true in
      Shtream.filter pred shtr

let renumber from =  
  trans 
    (let count = ref from in
       Shtream.map (fun line -> Line.set_seq !count line
                      BEFORE (count := !count + 1)) )
