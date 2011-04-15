#! /usr/bin/env ocamlscript
Ocaml.packs := [ "shcaml" ];;
--

(* This tests the ability of shtreams to remember the fd environment in
 * which their generators are to be run.  This should send a bunch of
 * meows to stdout, not stderr. *)

open Shcaml
open Channel.Dup
open Fitting

let f x = prerr_endline "meow"; Shtream.try_again ()
;;
let s = run_source begin
  from_file "/etc/passwd" -|
  sed f />/ [ 2 %>& 1 ]
end in
LineShtream.output s
