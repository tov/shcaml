#!/usr/bin/env ocaml

#use "topfind";;
#require "shcaml.top";;

let usage  = "[-s SECONDS] [COMMAND ARGS...]"
let lookup = Flags.go ~usage "-s <SECONDS:int>"
let delay  = lookup # int ~default:1 "-s"

let to_run =
  match lookup # strings "" with
  | prog :: args -> program prog args
  | _            -> command "yes i am going to run forever"

;;
run begin
  to_run ^&= fun proc ->
    sleep delay;
    Proc.kill ~raise:false Sys.sigint proc;
    yield (Proc.WEXITED 0)
end
