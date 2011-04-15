(** Protocol to discard the current continuation and replace it
 *  with a thunk.  In short, [E\[ abort M \] -> M ()]
 *
 * Often we want to fork a child process with a limited task, after
 * which it should exit.  The idea of {!Abort.abort} is that it enforces
 * that control does not escape the given thunk.  Furthermore, calling
 * {!Abort.set_abort} high up the stack enables memory that is no longer
 * in use to be recovered on the next GC.  (Obviously you don't want to
 * do this if you're about to exec, since you'll destroy copy-on-write
 * efficiency, but if you're going to fork and then stick around a
 * while, it might be worthwhile.)
 *)

(** Abort the current continuation, replacing it with a thunk.
    This function doesn't return.
    *)
val abort     : (unit -> unit) -> 'a

(** Set the abort handler if none is set.  Call this as soon as possible
 * in a program, so that the rest is in its dynamic extent, and then
 * {!Abort.abort} can throw control there. *)
val set_abort : (unit -> 'a) -> 'a
