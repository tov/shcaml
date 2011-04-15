#!/usr/bin/env ocaml

#use "topfind";;
#camlp4o;;
#require "shcaml";;

open Shcaml
open Fitting
open Channel.Dup
open Util
open UsrBin

let rm            = program "rm"
let mv src dest   = program "mv" [src; dest]

let curl url file = program "curl" [url] -| to_file file

let curl2 url1 url2 file =
  let file1 = file ^ ".1" in
  let file2 = file ^ ".2" in
  curl url1 file1 ^&= fun proc1 ->
  curl url2 file2 ^&= fun proc2 ->
  let winner = Proc.wait_any [proc1; proc2] in
  if winner == proc1 then begin
    Proc.kill ~raise:false Sys.sigint proc2;
    rm [file2] ^>>
    mv file1 file
  end else begin
    Proc.kill ~raise:false Sys.sigint proc1;
    rm [file1] ^>>
    mv file2 file
  end ^>>
  yield (Proc.wait winner)

let curl2 url1 url2 file =
  let file1 = file ^ ".1" in
  let file2 = file ^ ".2" in
  curl url1 file1 ^&= fun proc1 ->
  curl url2 file2 ^&= fun proc2 ->
  let winner = Proc.wait_any [proc1; proc2] in
  match Proc.wait winner with
   | Proc.WEXITED 0 when winner == proc1 ->
       Proc.kill ~raise:false Sys.sigint proc2;
       rm [file2] ^>>
       mv file1 file
   | Proc.WEXITED 0 when winner == proc1 ->
       Proc.kill ~raise:false Sys.sigint proc1;
       rm [file1] ^>>
       mv file2 file
   | _ when winner = proc1 ->
       let status = Proc.wait proc2 in
       rm [file1] ^>>
       mv file2 file ^>>
       yield status
   | _ ->
       let status = Proc.wait proc1 in
       rm [file2] ^>>
       mv file1 file ^>>
       yield status

let amb act1 act2 kont =
  act1 ^&= fun proc1 ->
  act2 ^&= fun proc2 ->
  let winner = Proc.wait_any [proc1; proc2] in
  match Proc.wait winner with
   | Proc.WEXITED 0 as status when winner == proc1 ->
       Proc.kill ~raise:false Sys.sigint proc2;
       kont 1 status
   | Proc.WEXITED 0 as status ->
       Proc.kill ~raise:false Sys.sigint proc1;
       kont 2 status
   | _ when winner == proc1 ->
       kont 2 (Proc.wait proc2)
   | _ ->
       kont 1 (Proc.wait proc1)

let curl2 url1 url2 file =
  let file1 = file ^ ".1" in
  let file2 = file ^ ".2" in
  amb (curl url1 file1) (curl url2 file2) ^$ fun winner status ->
  if winner = 1 then
    ~>>[ rm [file2]; mv file1 file; yield status ]
  else
    ~>>[ rm [file1]; mv file2 file; yield status ]

(**)

let shells =
  from_file "/etc/passwd"       -|
  Adaptor.Passwd.fitting ()     -|
  cut Line.Passwd.shell         -|
  sort ()                       -|
  uniq ()

(**)

let passwd_to_csv () =
  Shtream.iter (Line.Delim.output stdout) ^$
  Shtream.map
    (fun line -> Line.Delim.set_options Delimited.default_options ^$
       Line.Delim.create [| Line.Passwd.name line;
                            Line.Passwd.gecos line |] line) ^$
  Adaptor.Passwd.adaptor ^$
  LineShtream.of_file "/etc/passwd"

;;
passwd_to_csv ()
