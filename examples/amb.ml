#!/usr/bin/env ocamlscript

(* This script demonstrates how Shcaml can be used to implement the AMB
 * operator -- it runs two processes in the background, and when one
 * completes, it kills the other. *)

Ocaml.packs := [ "shcaml" ]
--

open Shcaml
open Fitting
open Util

let rec bg_list commands kont =
  match commands with
  | []      -> kont []
  | x :: xs -> command x ^&= fun proc ->
               bg_list xs @@ fun procs ->
               kont (proc :: procs)

let main args = ignore @@ run begin
  bg_list args @@ fun procs ->
  ignore @@ Proc.wait_any procs;
  List.iter (Proc.kill ~raise:false 9) procs;
  yield (Proc.WEXITED 0)
end

(* Another example (without the recursion). *)
let two_sleeps () = run begin
  command "sleep 1; echo a" ^&= fun a ->
  command "sleep 1; echo b" ^&= fun b ->
  Proc.kill ~raise:false 9
    (if a == Proc.wait_any [a; b] then b else a);
  yield (Proc.WEXITED 0)
end

if not !Sys.interactive then
  main ((Flags.go "") # strings "")
