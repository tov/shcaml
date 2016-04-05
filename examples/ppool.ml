#!/usr/bin/env ocaml

(* Keep a given number of processes running simultaneously *)

#use "topfind";;
#require "shcaml.top";;

let args = Flags.go ~usage:"-n NPROCS COMMAND ARGS..."
                    "-n <nprocs:int>";;
let n    = args # int "-n";;

match args # strings "" with
| prog :: args ->
    let reader = StringShtream.elem_reader
                   (Reader.make (`Set " \r\n\t")) in
    let each running next_proc =
      let running = 
        if List.length running < n
        then running
        else List.filter ((!=) @@ Proc.wait_any running) running in
      let proc = fst @@ Channel.open_program ~dups:[0 %<* `Null] 
                          prog (args @ [next_proc]) in
        print_endline (string_of_int (Proc.pid_of_proc proc));
        proc :: running in
    let shtream = StringShtream.of_channel ~reader stdin in
      ignore (Shtream.fold_left each [] shtream)
| _ ->
    Channel.dup2 ( 1 %>& 2 );
    args # usage;
    exit 2
