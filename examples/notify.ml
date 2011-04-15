#!/usr/bin/env ocaml

#use "topfind";;
#require "shcaml";;

open Channel
open Shtream
open Util

let gettime args = 
  Int64.of_string $ 
    backquote ("date -d " ^ args ^ " +%s")

let poor_man's_daemonize () =
  maybe (Proc.fork ()) id (fun _ -> exit 0)

let main time = 
  let now   = gettime "now" in
  let later = gettime time in
  let delay = Int64.to_int (Int64.sub later now) in
  if delay < 0
    then prerr_endline "Can't remind you of a date in the past"
    else begin
      poor_man's_daemonize ();
      mov2 (1 %>/ open_command_out "xmessage -file -");
      sleep delay;
      print_endline
        (if Array.length Sys.argv < 3
           then "Consider yourself notified"
           else Sys.argv.(2))
    end

;;
if Array.length Sys.argv < 2 then begin
  Printf.eprintf "Usage: %s DELAY [MESSAGE]\n" Sys.argv.(0);
  exit 2
end else
  main Sys.argv.(1)
