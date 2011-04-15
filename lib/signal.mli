(**
 * Treat UNIX signals as Ocaml exceptions.
 *)

(** Raised by {!signal_protect} when a signal is delivered. *)
exception Signal of int

(** Call a thunk while delivering signals as exceptions.
 * [Signal.signal_protect n ~exn thunk] calls [thunk], handling
 * signal [n] by throwing exception [exn] instead.  If [?exn] isn't
 * given, it default to [Signal n]. *)
val signal_protect : int -> ?exn:exn -> (unit -> 'a) -> 'a

