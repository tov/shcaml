#!/usr/bin/env ocaml

#use "topfind";;
#require "shcaml.top";;

(** This is a quick one-shot to downsample all the mp3s in the current
    directory into a subdirectory called lofi/.  I actually do this
    all the time for audiobooks I'm going to listen to on my phone,
    but type it in by hand.  In shell, it's:

    for i in *mp3; do lame --preset sw "$i" lofi/"$i"; done
    
    This is a 
*)

let main () =
  let files = 
    run_source (command "ls" -|
                grep (Reader.ends_with ".mp3" % Line.show)) in
    mkpath "lofi";
    Shtream.iter (fun l -> 
                    let name = "'" ^ Line.show l ^ "'" in
                      ignore @@
                        Proc.system
                        ("lame --preset sw " ^ name ^ " lofi/" ^ name)) files

in
  main ()
