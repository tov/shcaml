open Util

exception Abort of (unit -> unit)

let abort_is_set = ref false

let rec with_abort kont =
  let result =
    try Right (kont ()) with
    | Abort next -> Left next in
  match result with
  | Right r   -> r
  | Left next -> with_abort
                  (fun () ->
                    try next (); exit 0 with
                    | Abort _ as a -> raise a
                    | e            -> exit 2)

let set_abort kont =
  if !abort_is_set 
  then kont ()
  else unwind_protect
         (fun () -> abort_is_set := true;
                    with_abort kont)
         (fun () -> abort_is_set := false)

let abort thunk =
  set_abort (fun () -> raise (Abort thunk))
