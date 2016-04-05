#!/usr/bin/env ocaml

#use "topfind";;
#require "shcaml.top";;

open Proc

let isn't_running prog = 
  Shtream.is_empty @@
    run_source begin
      ps () -|
      grep (Reader.starts_with prog % Line.Ps.command)
    end

let main prog args delay =
  if isn't_running prog && not (Test.d (backquote "date +%Y-%m-%d")) then
    let proc = vfork_program prog args in
      sleep delay;
      match status_of_proc proc with
      | None   -> ()
      | Some n ->
          Printf.eprintf ("%s stubbornly refuses to run\n%!") prog
;;

let lookup = Flags.go ~usage:"[--wait SECONDS] [--] PROGRAM [ARGS...]"
                      "--wait <SECONDS:int>" in
let delay  = lookup # int ~default:2 "--wait" in
match lookup # strings "" with
  | prog::args ->
      main prog args delay
  | _ ->
      lookup # usage;
      exit 1
