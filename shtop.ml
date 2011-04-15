(* Set up the Shtop environment *)

open Shcaml
open Shcaml.Util
open Shcaml.Channel.Dup
open Toploop

(* A very quiet formatter. *)
let silently = Format.make_formatter (fun _ _ _ -> ()) ignore

(* top_do takes a string and runs it in the toploop. *)
let top_do =
  ignore %
    execute_phrase false silently %
      !parse_toplevel_phrase %
        Lexing.from_string %
          (flip (^) ";;")

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

let print_shtream fmt shtream =
  match Shtream.status shtream with
  | None   -> Format.fprintf fmt "<shtream:active>"
  | Some 0 -> Format.fprintf fmt "<shtream:finished>"
  | Some n -> Format.fprintf fmt "<shtream:error %d>" n

let dir_shcaml () =
  top_do "open Shcaml";
  top_do "open Util";
  top_do "open UsrBin";
  top_do "open Fitting";
  top_do "open Channel.Dup";
  top_do "#install_printer Shtop.print_line";
  top_do "#install_printer Shtop.print_descr";
  top_do "#install_printer Shtop.print_proc";
  top_do "#install_printer Shtop.print_in_channel";
  top_do "#install_printer Shtop.print_out_channel";
  (* Printing shtreams actually has funny consequences, so let's not. *)
  (* top_do "#install_printer Shtop.print_shtream"; *)
  (*
  Channel.with_dups [ 1 %>* `Null ]
    {| top_do "#use \"topfind\"" |};
    *)
  if !Sys.interactive
    then Printf.printf "\tCaml-Shcaml version %s (%s)\n\n%!"
           Version.version Version.version_name;

;;
begin
  Hashtbl.add directive_table "shcaml" (Directive_none dir_shcaml);
  Hashtbl.add directive_table "shtop"  (Directive_none dir_shcaml);
end
