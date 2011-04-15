(** One-shot interprocess exceptions and variables. *)

(**
 * {2 Interprocess Exceptions}
 *)

(** Relay exceptions from a subprocess.
    [with_interprocess_protect kont] calls [kont] with one
    argument, [protect: (unit -> 'a) -> 'a].

    The function [kont] {b must} fork into an {i observer} process and an
    {i observed} process.  The observed process {b must not} return from the
    call to [kont]; it must, however, call [protect thunk] exactly
    once with some thunk, which [protect] will call.  When [kont]
    returns in the observer process, it blocks until the thunk
    returns.  If the thunk returns normally (or execs, or exits), then
    [protect thunk] returns the result of the thunk in the observed
    process, and [with_interprocess_protect] returns the result of
    [kont] in the observer process.  However, if the thunk raises an
    exception, then the observed process terminates with status 2 and
    the call to [with_interprocess_protect] in the {i observer}
    returns abnormally by re-raising the exception.

    In this example, if {!Proc.exec} raises an exception in the child process,
    then [with_interprocess_protect] will re-raise that exception in
    the parent process:

{[
  with_interprocess_protect
    (fun protect ->
       match Proc.fork () with
       | None     ->
           protect (fun () -> Proc.exec prog args);
           exit 3 (* can't happen *)
       | Some proc -> proc)
]}
 *)
val with_interprocess_protect
           : (((unit -> 'a) -> 'a) -> 'b) -> 'b

(** Relay exceptions from another process.
    [with_interprocess_raise_and_okay kont] calls [kont] with two
    arguments, [{
      oops : exn -> unit
      okay : unit -> unit
    }]

    The function [kont] {b must} fork into an {i observed} process and an
    {i observer} process.  When the call to [kont] returns in
    the {i observer} process, [with_interprocess_raise_and_okay] then
    waits for either [oops] or [okay] to be called in the {i observed}
    process.  If [okay ()] is called, then it returns the result of
    [kont]; if [oops e] is called, then it instead raises the exception
    [e] in the {i observer} process.  If the observed process fails to call
    either [oops] or [okay], then the observer process will block
    indefinitely.  *)
val with_interprocess_raise_and_okay
  : ((exn -> unit) -> (unit -> unit) -> 'b) -> 'b

(**
 * {2 Interprocess Variables}
 *)

type 'a read_end
(** The read-end of an interprocess variable. *)
type 'a write_end
(** The write-end of an interprocess variable. *)

(** Raised on attempts to re-use an {!IVar}.
    IVars allow (require, in fact) exactly one read and one write.
*)
exception Dead

(** Create a channel pair [(r, w)].  The protocol is then as
    follows.  One process must execute:
    - [read r]

    This operation will block, until another process does one of:
    -   [write w v] :    read returns [Some v]
    -   [close w] :      read returns [None]
    -   [exec ...] :     read returns [None]
    -   [exit ...] :     read returns [None ]

    The {!write} call may or may not block, depending on the underlying
    implementation.  In any case, it is {b imperative} that read
    happens in a separate process from the write/close/exec/exit, or
    the program may block indefinitely. *)
val create : unit -> 'a read_end * 'a write_end

(** Read an ['a option] from an {!IVar}.
    Blocks until the associated {!write_end} is written or closed.
    If [x] is written on the other end, returns [Some x]; if
    the other end is closed (including by exit or exec), returns
    [None].
    *)
val read   : 'a read_end -> 'a option

(** Write to an {!IVar}. *)
val write  : 'a write_end -> 'a -> unit

(** Close an {!IVar} without writing. *)
val close  : 'a write_end -> unit
