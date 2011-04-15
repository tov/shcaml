#! /usr/bin/env ocamlscript
Ocaml.packs := [ "shcaml" ]
--

open Shcaml
open Fitting

let echo s = program "echo" [s];;

run begin
  ( command "sleep 1"   ^>>
    echo "b"            ^>>
    command "sleep 1" ) ^&= function proc ->
  echo "a"              ^>>
  caml {|
    ignore (Proc.wait proc);
    echo "c"
  |}
end
