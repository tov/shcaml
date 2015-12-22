(* Set up the Shtop environment *)

open Shcaml
open Shcaml.Util
open Shcaml.Channel.Dup
open Toploop

(* A very quiet formatter. *)
let silently = Format.make_formatter (fun _ _ _ -> ()) ignore

(* Some helpful printers: *)

let print_line fmt line =
  Format.fprintf fmt "<line:\"%s\">" (String.escaped (Line.show line))

let print_descr fmt descr =
  Format.fprintf fmt "<descr:%d>" (Channel.fd_of_descr descr)

let print_proc fmt proc =
  Format.fprintf fmt "<proc:%d %s>" (Proc.pid_of_proc proc)
    (if Proc.is_child proc
     then match Proc.status_of_proc proc with
      | None                    -> "running"
      | Some (Unix.WEXITED n)   -> Format.sprintf "exited:%d" n
      | Some (Unix.WSIGNALED n) -> Format.sprintf "signaled:%d" n
      | Some (Unix.WSTOPPED n)  -> Format.sprintf "stopped:%d" n
     else "non-child")

let print_in_channel fmt ic =
  Format.fprintf fmt "<in_channel:%d>"
    (Channel.fd_of_descr (Channel.descr_of_gen (`InChannel ic)))

let print_out_channel fmt oc =
  Format.fprintf fmt "<out_channel:%d>"
    (Channel.fd_of_descr (Channel.descr_of_gen (`OutChannel oc)))

let dir_shcaml () =
  Topdirs.dir_use silently "dir_shcaml.ml";
  if !Sys.interactive
  then Printf.printf "\tCaml-Shcaml version %s (%s)\n\n%!"
      Version.version Version.version_name
