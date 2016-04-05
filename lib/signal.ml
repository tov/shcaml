open Util

exception Signal of int

let signal_protect which ?(exn = Signal which) thunk =
  let old = Sys.signal which (Sys.Signal_handle (fun _ -> raise exn)) in
  unwind_protect thunk (fun _ -> ignore @@ Sys.signal which old)
