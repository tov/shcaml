(** An Ocaml abstraction for UNIX processes.
    The {!Proc} module takes responsiblity for reaping
    children and provides access to exit codes through abstract
    {!Proc.t} objects.  (If you need to reap yourself,
    {!Proc.don't_autoreap} will turn off the [Sys.sigchld] handler, and
    {!Proc.autoreap} will turn it back on.)

    Because the {!Proc} module is responsible for reaping, it makes
    exit status available as many times as necessary, though
    {!Proc.wait} and {!Proc.status_of_proc}.

    It's also possible to construct a {!Proc.t} object from a pid
    ([int]) even if the process wasn't created using this library.  In
    this case, the library detects whether the process is a child or
    not.  We don't allow waiting
    on or getting the status of non-child processes, because UNIX
    doesn't.
    
    Much of this design is due to Cash/Scsh.
*)


(**
{2 Types}
*)

(** Raised on attempts to get the exit status of a process that
    isn't a child of the current process *)
exception Not_child

(** The abstract type of a process representation *)
type t

(** Alias for [Unix.process_status] *)
type status = Unix.process_status =
  | WEXITED of int
  (** The process terminated normally by exit; the argument is the
      return code. *)
  | WSIGNALED of int
  (** The process was killed by a signal; the argument is the
      signal number. *)
  |  WSTOPPED  of int
  (** The process was stopped by a signal; the argument is the
      signal number. *)

(**
{2 Process Management}
*)

(** Return [Some t] in the parent and [None] in the child *)
val fork                : unit -> t option

(** Run a thunk in a subprocess, returning its {!Proc.t}.  {!spawn}
    will not allow control in the subprocess to return to its caller; to
    this end, it catches all exceptions, printing a message and then
    exiting with status 2.  The optional argument [?quiet] (default
    false) suppresses the message. *)
val spawn               : ?quiet:bool -> (unit -> unit) -> t

(** Send a signal to a process.  [Proc.kill n p] sends signal [n]
    to process [p].  The optional argument [?raise] (default [true])
    specifies whether to raise an exception if the process doesn't
    exist or we aren't allowed to kill it.

    Raises [Unix.Unix_error] (see [Unix.kill])
  *)
val kill                : ?raise:bool -> int -> t -> unit

(** [Proc.wait proc] performs a blocking wait on [proc];
    if the child has already exited, it returns immediately.
    Unlike [Unix.waitpid], [Proc.wait] may
    be called multiple times for the same process.  If [proc] is not a child
    of the calling process, raises {!Not_child}. *)
val wait                : t -> status

(** Given a list of [Proc.t]s, return any one of them that has exited.
    If one has exited already, it return immediately, but if all are still
    running, it blocks.  Calling {!Proc.wait_any} may reap children
    other than those in the list.
    
    Raises [Not_child] if given the empty list or any non-children. *)
val wait_any            : t list -> t

(** Retrieve the status of a process if it has exited; non-blocking.
    Raises {!Not_child} if [proc] is not a child of the calling
    process. *)
val status_of_proc      : t -> status option    (* raises Not_child *)

(** Is a process a child of the calling process? *)
val is_child            : t -> bool

(** The UNIX process ID associated with [proc] *)
val pid_of_proc         : t -> int

(** Find or create a {!Proc.t} associated with a UNIX
process.  If there is no {!t} but the process exists, it constructs
    one.  Raises [Not_found] if there is no process with the given process id,
    or [Invalid_argument "procs_of_pid"] if the pid is non-positive.
    *)
val proc_of_pid         : int -> t              (* raises Not_found *)

(** Returns a list of all {!Proc.t}s with the given process id.
    There may be more than one if the same process id has been used
    multiple times (rarely). *)
val procs_of_pid        : int -> t list


(** Exit with the given exit status.  If the status indicates a
    signal, this function sets the default signal handler and signals
    the current process. *)
val exit_with_status    : status -> 'a

(**
{3 Autoreaping}
*)

(** Turn on autoreaping of processes.  When set, Shcaml will
    automatically wait on processes and store their exit status
    for retrieval by {!wait} or {!status_of_proc}. *)
val autoreap            : unit -> unit

(** Turn off autoreaping of processes.  If autoreaping is disabled,
    {!wait} and {!status_of_proc} will still work, but the user is
    responsible to reap all processes. *)
val don't_autoreap      : unit -> unit

(**
{2 Running Programs}
*)

(** Run a command and wait for it to exit.
    Passes the command to the shell for parsing.
    If the shell cannot be found or run, raises
    the same exceptions as [Unix.execv].
    
    This function delegates to the shell for argument parsing; if you
    already have a list, use {!system_program}.
    *)
val system              : string -> status

(** Run a program with arguments and wait for it to exit.
    Optional argument [?path] (default [true]) specifies whether
    to search the path, and [?argv0] (default [prog]) specifies
    an alternate value for the new process's [argv.(0)].

    If [Unix.execv] raises an exception in the child process,
    [Proc.system_program] re-raises the exception on the parent process.

    This function takes an already-parsed argument list.  To have the
    shell do it, use {!system}.
    *)
val system_program      : ?path:bool -> string ->
                          ?argv0:string -> string list -> status

(** Run a command asynchonously.  Like {!system}, but doesn't wait. *)
val vfork               : string -> t

(** Run a program asynchonously.  Like {!system_program}, but doesn't wait. *)
val vfork_program       : ?path:bool -> string ->
                          ?argv0:string -> string list -> t

(** Replace the current process image with a command.
    Like {!vfork}, but doesn't fork. *)
val exec                : string -> 'a

(** Replace the current process image with another.
    Like {!vfork_program}, but doesn't fork. *)
val exec_program        : ?path:bool -> string ->
                          ?argv0:string -> string list -> 'a

(**
 * {3 The [execspec] Record. }
 *)

(** Several Shcaml functions (such as {!exec_program} and
   {!system_program}) take the same arguments to specify a
   program to execute.  It's sometimes
   helpful to package these arguments in a record and pass them to such
   a function later.  *)
type execspec = { path:    bool option; 
                  (** Search the path (default [true]) *)
                  program: string;
                  (** Executable to run *)
                  argv0:   string option;
                  (** Zeroth argument (default [program]) *)
                  args:    string list; 
                  (** Additional arguments *)
                }

(** Constructs an {!execspec}, given the same arguments as
 * {!exec_program}. *)
val execspec      : ?path:bool -> string ->
                    ?argv0:string -> string list ->
                    execspec

(** Call a function with a given {!execspec}.
    For example,
    [Proc.with_execspec (Proc.execspec ~path program ~argv0 args) f] calls
    [f ~path program ~argv0 args]. *)
val with_execspec : execspec ->
                    (?path:bool -> string ->
                     ?argv0:string -> string list -> 'a) ->
                    'a

(**
{2 Pretty-printing}
*)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for {!Proc.t}. *)
