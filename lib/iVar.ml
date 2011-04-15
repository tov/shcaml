open Util

exception Dead

type rep = {
  read_end     : Unix.file_descr;
  write_end    : Unix.file_descr;
  mutable dead : bool;
}

type 'a read_end  = rep
type 'a write_end = rep

let create () =
  let r, w = Unix.pipe () in
  Unix.set_close_on_exec r;
  Unix.set_close_on_exec w;
  let rep  = {
    read_end  = r;
    write_end = w;
    dead      = false;
  } in
    rep, rep

(* This isn't thread safe, but threads shouldn't communicate with one-shot
 * IVars anyway. *)
let kill rep =
  if rep.dead
  then raise Dead
  else rep.dead <- true

let write rep v =
  kill rep;
  Unix.close rep.read_end;
  let buf = Marshal.to_string v [Marshal.Closures] in
  ignore ^$ Unix.write rep.write_end buf 0 (String.length buf);
  Unix.close rep.write_end

let read rep =
  kill rep;
  Unix.close rep.write_end;
  let c = Unix.in_channel_of_descr rep.read_end in
  let v = try Some (Marshal.from_channel c)
          with End_of_file -> None in
  Pervasives.close_in c;
  v

let close rep =
  kill rep;
  Unix.close rep.write_end;
  Unix.close rep.read_end

let with_interprocess_raise_and_okay kont =
  let r, w = create () in
  let result = kont (fun e -> write w e) (fun () -> close w) in
    match read r with
    | None   -> result
    | Some e -> raise e

let with_interprocess_protect here =
  with_interprocess_raise_and_okay
    (fun throw ok ->
      here (fun there ->
             try let r = there () in
                   ok (); r
             with e -> throw e; exit 2))

