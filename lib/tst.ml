(** Simple-minded testing script that treats every executable file in
    a directory as a test and interprets its exit code/output as the
    result of the test.  *)

open Util
open UsrBin
open Fitting

type result = Pass | Fail | XPass | XFail | InOutput | Unresolved

let get_exes () =
  Shtream.list_of %
    Shtream.map ((^) "./") %
    Shtream.filter (Test.test (`And `Execute `Reg))
    $ Shtream.map Line.raw (ls ".")

let result_of_exit code =
  match code with
      0 -> Pass
    | 1 -> Fail
    | 2 -> XPass
    | 3 -> XFail
    | 4 -> InOutput
    | _ -> Unresolved

let exit_of_result res = 
  match res with
      Pass -> 0
    | Fail -> 1
    | XPass -> 2
    | XFail -> 3
    | InOutput -> 4
    | Unresolved -> 5

let snarf_channel c = 
  let b = Buffer.create 80 in
  let rec loop () = 
    try 
      Buffer.add_string b (input_line c);
      Buffer.add_char b '\n';
      loop ()
    with End_of_file -> 
      let s = Buffer.contents b in
        close_in c; s
  in loop ()
  
let run_test prog =
  let proc   = ref None in
  let outc   = run_in ~procref:proc (process ~path:false prog []) in
  let output = snarf_channel outc in
    (prog, 
     (maybe !proc 
        {| Unresolved |} 
        (fun p ->
           match Proc.wait p with
               Unix.WEXITED n -> result_of_exit n
             | _              -> Unresolved)), output)
        
    (*
       (* Toploop is messing with me, and this code appears to be dead
          anyway. *)
let is_compileable file = 
  Toploop.use_file 
    (Format.make_formatter (fun _ _ _ -> ()) ignore) 
    file

let test_compiles file = 
  exit (exit_of_result $
          if is_compileable file
          then Pass
          else Fail)
  *)

let run_tests dir =
  let oldir = Unix.getcwd () in
    Unix.chdir dir;
    List.map run_test (get_exes ())
      BEFORE
      Unix.chdir oldir 

(*************************************)
open Arg

if not !Sys.interactive then
  let dir = ref "." in
  let speclist = align
    (* key          spec           doc *)
    [
    ] in
    parse speclist (fun x -> dir := x)  (Sys.argv.(0) ^ " [dir]")
