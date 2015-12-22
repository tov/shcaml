#!/usr/bin/env ocaml

(* Quick reimplementation of pgrep(1). *)

#use "topfind";;
#require "shcaml.top";;

let pgrep pat = ignore @@ run begin
  ps () -|
  grep (Reader.starts_with pat % Line.Ps.command) -|
  cut (string_of_int % Line.Ps.pid)
end

;;
if Array.length Sys.argv > 1 then
  pgrep Sys.argv.(1)
else
  Printf.eprintf "Usage: %s PROGRAM\n"  Sys.argv.(0)
