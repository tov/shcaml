(** Evaluates dependency DAGs of processes in parallel. *)

(** A task is specified as a thunk that starts a process. *)
type task = unit -> Proc.t

(** A DAG of tasks. *)
type t

(** Make a DAG whose goal is a task, given a list of prereqisite DAGs.
  * Takes an optional priority, used to decide which tasks to schedule
  * first; lower integers are scheduled first, and default is 0. *)
val make : ?prio:int -> task -> t list -> t

(** Make a DAG whose goal comprises all the goals in the list. *)
val make_par : ?prio:int -> t list -> t

(** Run a DAG with the specified parallelism.  Default [n] is 0. *)
val run : ?n:int -> t -> unit
