#!/usr/bin/env ocaml

#use "topfind";;
#require "shcaml.top";;

open Printf;;

let lookup = Flags.go "-a --bee -c <Cat> --dog=<Dog:int>" in
            
printf "Option -a is %b\n" (lookup#bool "-a");
printf "Option --bee is %b\n" (lookup#bool "--bee");

printf "Option -c is %s\n" (lookup#string ~default:"<nothing>" "-c");
printf "Option --dog is %d\n" (lookup#int ~default:(-1) "--dog");

printf "All -c are:\n";
List.iter print_endline (lookup#strings "-c");
print_newline ();

printf "All --dog are:\n";
List.iter (fun n -> print_endline (string_of_int n))
          (lookup#ints "--dog");
print_newline ();

printf "Trying to lookup string --dogs:\n";
List.iter print_endline (lookup#strings "--dog");
print_newline ();

printf "Other arguments are:\n";
List.iter print_endline (lookup#strings "");
print_newline ();

printf "Finally, usage:\n";
lookup#usage

