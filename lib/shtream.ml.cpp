(* vim: set ft=ocaml : *)
open Util
open Channel

(* *)

exception Failure

exception FailWith of int
exception TryAgain
exception Warning of string

(*
 * Representation
 *)
type 'a data =
  | Strict of 'a * 'a data
  | Delay  of (unit -> 'a data)
  | Extern of in_channel
  | TheEnd of int

type 'a t = {
  mutable data:     'a data;
  mutable read:     in_channel -> 'a;
  mutable hint:     (Reader.raw_line -> 'a) option;
  mutable close:    unit -> unit;
  mutable procref:  procref option;
  mutable protect:  'b . (unit -> 'b) -> 'b;
}

type protector       = Util.protector
let null_protect f   = f ()
let null_protect_rec = { protector = null_protect; }

(*
 * Stream initialization and finalization
 *)

let make ?(read = {| raise Bug |})
         ?(hint = None)
         ?(close = ignore)
         ?procref
         ?(protect = null_protect_rec)
         data =
  { data      = data;
    read      = read;
    hint      = hint;
    close     = close;
    procref   = procref;
    protect   = protect.protector; }

(*
 * Public functions (from Stream)
 *)

(* We keep the position in a ref cell rather than passing it along
 * functionally because Shtream.force (below) will call our thunk
 * multiple times if TryAgain is raised, and we want the index to
 * advance on retries. *)
let from_low ?close f =
  let position = ref 0 in
  let n ()     = !position BEFORE (position := it + 1) in
  let rec loop = Delay {| Strict (f (n ()), loop) |} in
    make ?close loop

let from f =
  from_low (fun n -> maybe (f n) {| raise Failure |} id)

let try_again () = raise TryAgain
let warn fmt     = Printf.ksprintf (fun s -> raise (Warning s)) fmt
let fail_with n  = raise (FailWith n)

let close s =
  (match s.data with
   | TheEnd _ -> ()
   | _        -> s.data <- TheEnd 0);
  s.close ();
  s.close <- ignore

let set_reader s r =
  s.read <- r;
  s.hint <- None

let hint_reader s r =
  match s.hint with
  | Some h -> s.read <- h % r
  | _      -> ()

let add_protection protector s =
  if s.protect == null_protect
  then s.protect <- protector.protector
  else s.protect <- let old = s.protect in
                    fun thunk -> protector.protector {| old thunk |}

let add_cleanup close s =
  if s.close == ignore
  then s.close <- close
  else s.close <- let old = s.close in {| old (); close () |}

let of_list lst =
  make (List.fold_right (fun x y -> Strict (x, y)) lst (TheEnd 0))

let of_channel ?hint read c =
  let c = dup_in (`InChannel c) in
  make ~read ~close:{| close_in c |} ~hint (Extern c)

let of_stream stream =
  let rec loop = Delay (fun () ->
    try Strict (Stream.next stream, loop)
      with Stream.Failure -> TheEnd 0) in
  make loop

(* Error handling *)

type error_handler = [`Warning of string | `Exception of exn] -> unit 

let string_of_shtream_error = function
  | `Warning w   -> w
  | `Exception e -> Printexc.to_string e

let ignore_errors = ignore

let warn_on_errors e =
  Printf.eprintf "%s: shtream warning: %s\n%!" Sys.argv.(0) ^$
    string_of_shtream_error e

let die_on_errors e =
  Printf.eprintf "%s: shtream error: %s\n%!" Sys.argv.(0) ^$
    string_of_shtream_error e;
  raise (FailWith (-1))

let die_silently_on_errors _ = raise (FailWith (-1))

let current_error_handler = ref warn_on_errors

(* Shtream evaluation *)

(* When called with N >= 0, will produce a shtream with N
 * Stricts in front or fewer than N followed by TheEnd. *)
LOCAL
  let end_of_procref = function
    | Some {contents = Some proc} -> begin
        match Proc.status_of_proc proc with
        | Some (Unix.WEXITED m)   -> TheEnd m
        | Some (Unix.WSIGNALED m) -> TheEnd (1024 + m)
        | _                       -> TheEnd 0
      end
    | _ -> TheEnd 0

  let call_error_handler = function
    | Warning s -> !current_error_handler (`Warning s)
    | e         -> !current_error_handler (`Exception e)

  let finish s =
    close s;
    end_of_procref s.procref
    BEFORE (s.procref <- None)

  let finish_with n s =
    close s;
    s.procref <- None;
    TheEnd n

  let handle s retry = function
    | End_of_file
    | Failure    -> finish s
    | FailWith m -> finish_with m s
    | TryAgain   -> retry
    (* Some exceptions should be passed upward.  What others? *)
    | Sys.Break as e
                 -> raise e
    | e          ->
        try call_error_handler e; retry with
        | Failure    -> finish s
        | FailWith m -> finish_with m s
        | TryAgain   -> retry

  let rec loop s n d = match d with
   | _ when n = 0    -> d
   | Strict (x, y)   -> Strict (x, loop s (n - 1) y)
   | Delay f         -> loop s n (try s.protect f with
                                  | e -> handle s d e)
   | Extern c        -> loop s n (try Strict (s.read c, d) with
                                  | e -> handle s d e)
   | TheEnd n        -> close s; TheEnd n
IN
  let force n s = s.data <- loop s n s.data
END

let npeek ?(n = 1) s =
  let rec loop n d = match d with
   | _ when n = 0    -> []
   | Strict (x, y)   -> x :: loop (n - 1) y
   | TheEnd _        -> []
   | _               -> raise Bug
  in force n s;
     loop n s.data

let peek ?(n = 0) s =
  let rec loop n d = match d with
   | Strict (r, _) when n = 0 -> Some r
   | Strict (_, y)            -> loop (n - 1) y
   | TheEnd _                 -> None
   | _                        -> raise Bug
  in force (n + 1) s;
     loop n s.data

let junk ?(n = 1) s =
  let rec loop n d = match d with
   | _ when n = 0    -> d
   | Strict (_, y)   -> loop (n - 1) y
   | TheEnd _        -> d
   | _               -> raise Bug
  in force n s;
     s.data <- loop n s.data

let next s = match peek s with
 | Some r -> junk s; r
 | None   -> raise Failure

let next' s = match peek s with
 | r      -> junk s; r

let empty s = match next' s with
 | None   -> ()
 | _      -> raise Failure

let is_empty s = match next' s with
 | None   -> true
 | _      -> false

let status s = 
  ignore (peek s);
  match s.data with
 | TheEnd n -> Some n
 | _        -> None

let insert element s =
  s.data <- Strict (element, s.data)

let cons element s =
  insert element s;
  s

let nil () = make (TheEnd 0)

let rec iter f s = match next' s with
 | Some r -> f r; iter f s
 | None   -> ()

(*
 * Public functions (new to Shtream)
 *)

(* To enforce (dynamic) linearity of shtreams, this function creates
 * a copy of s and then empties s. *)
let claim s =
  { s with read = s.read } BEFORE begin
    s.data    <- TheEnd 0;
  end

let append s1 s2 =
  let s1, s2 = claim s1, claim s2 in
  from_low ~close:{| close s1; close s2 |}
    {| match next' s1 with
       | None   -> next s2
       | Some r -> r |}

let filter pred s =
  let s = claim s in
  let rec each n =
    let x = next s in
      if pred x then x else each n in
  from_low ~close:{| close s |} each

let map trans s =
  let s = claim s in
    from_low ~close:{| close s |} {| trans (next s) |}

let concat_map trans s =
  let rec data = Delay {|
    List.fold_right (fun x y -> Strict (x, y)) (trans (next s)) data
  |} in
  make ~close:{| close s |} data

let partition pred left right =
  map (fun x -> if pred x then left x else right x)

let rec fold_left f z s =
  match next' s with
  | None   -> z
  | Some x -> fold_left f (f z x) s

let rec fold_right f s z =
  match next' s with
  | None   -> z
  | Some x -> f x (lazy (fold_right f s z))

let stream_of s =
  let s = claim s in
  Stream.from {| next' s |}

let list_of s =
  let rec loop acc = match next' s with
   | Some x -> loop (x :: acc)
   | None   -> acc in
  List.rev (loop [])

let channel_of ?procref ?(before = ignore) ?(after = ignore) write s =
  let rec loop = function
    | Extern c   -> dup_in (`InChannel c)
    | Delay f    -> loop (f ())
    | data       -> s.data <- data;
                    open_thunk_in ?procref {|
                      before ();
                      flush stdout;
                      iter (fun each ->
                              write each;
                              flush stdout) s;
                      after ();
                      flush stdout;
                      match status s with
                      | Some n -> exit n
                      | _      -> exit 0
                    |} in
  let result = loop s.data in
    close s;
    result

let of_channel_with_close ?hint reader c =
  unwind_protect
    {| of_channel ?hint reader c |}
    {| close_in c |}

let of_file ?hint reader filename =
  of_channel_with_close ?hint reader (open_file_in filename)

let of_command ?(procref = ref None) ?dups ?hint reader command =
  of_channel_with_close ?hint reader
    (open_command_in ~procref ?dups command)
  BEFORE (it.procref <- Some procref)

let of_program ?(procref = ref None) ?dups ?hint reader
               ?path prog ?argv0 args =
  of_channel_with_close ?hint reader
    (open_program_in ~procref ?dups ?path prog ?argv0 args)
  BEFORE (it.procref <- Some procref)


let of_thunk ?(procref = ref None) ?dups ?hint reader thunk =
  of_channel_with_close ?hint reader
    (open_thunk_in ~procref ?dups thunk)
  BEFORE (it.procref <- Some procref)

exception CoFailure

type 'a co_t = out_channel

let coshtream_of ?procref consumer =
  open_thunk_out ?procref
    {| let each _ = try Marshal.from_channel stdin
                    with End_of_file -> raise Failure in
       consumer (from_low each);
       exit 0 |}

let conil = null_out

let unsafe_conext c v =
  let buf = Marshal.to_string v [Marshal.Closures] in
    output_string c buf;
    flush c

let sigpipe_protect thunk =
  Signal.signal_protect Sys.sigpipe ~exn:CoFailure thunk

let conext c v =
  sigpipe_protect {| unsafe_conext c v |}

let coclose c =
  try sigpipe_protect {| close_out c |}
  with CoFailure -> ()

let annihilate shtream coshtream =
  begin
    try sigpipe_protect {| iter (unsafe_conext coshtream) shtream; |}
    with CoFailure -> ()
  end

module type COMMON = sig
  exception Failure
  type 'a t
  exception CoFailure
  type 'a co_t
  #include "shtream.sig"
end
