(*
 * The idea of Proc is to abstract away UNIX processes into something
 * more manageable from Ocaml.  To this end, we create abstract Proc.t
 * objects that we store in a weak table; a SIGCHLD handler collects the
 * exit status of child processes and stores them in the Proc.t objects
 * in this table, when possible.
 *
 * A call to Proc.wait then looks in the table for exit status (and thus
 * may be called more than once).  If the child process hasn't exited
 * yet, then Proc.wait actually calls UNIX waitpid to get the exit
 * status.
 *
 * It's also possible to construct a Proc.t object from a pid (int) even
 * if the process wasn't created using this library.  In this case, the
 * library detects whether the process is a child or not, and records
 * this fact in the record.  We don't allow waiting on or getting the
 * status of non-child processes, for obvious reasons.  We also take
 * care in Proc.fork that in the new child process, all the procs in the
 * table are marked as non-children, since they are siblings of the new
 * process.
 *)
open Util

exception Not_child

type status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type t = {
  pid            : int;
  mutable status : status option;
  mutable child  : bool;
}

type proc = t

(* In the weak table, we look up proc records just by the pid, since we
 * don't know ahead of time what the status will be.  Thus, when we call
 * the helper find_pid (below) to find all procs with a given pid, we
 * just use dummy data for the other two fields. *)
module Table = Weak.Make(struct
  type t = proc
  let hash proc         = Hashtbl.hash proc.pid
  let equal proc1 proc2 = proc1.pid = proc2.pid
end)

let table = Table.create 128

let find_pid pid = Table.find_all table
                     { pid = pid; status = None; child = true }

(* When a process exits and we want to record its exit status, we don't
 * just want the Proc.t with the right pid, but we want one that doesn't
 * have an exit status yet.  PIDs can be recycled, but this lets us keep
 * our association exact (hopefully!). *)
let unwaited_proc_of_pid pid =
  try Some (List.find (fun proc -> proc.status = None) (find_pid pid))
  with Not_found -> None

let stash_status pid status =
  match unwaited_proc_of_pid pid with
  | None      -> ()
  | Some proc -> proc.status <- Some status

let rec wait_and_save ?(pid = -1) flags =
  try
    match Unix.waitpid flags pid with
    | 0,   _      -> raise Not_found
    | pid, status -> stash_status pid status
  with Unix.Unix_error (Unix.EINTR, "waitpid", _)
    -> wait_and_save ~pid flags

(* We need to block SIGCHLD while forking, because otherwise there's a
 * race condition between the child exiting and the parent adding the
 * child to its known process table.
 *)
let fork () =
  Pervasives.flush_all ();
  let old_mask   = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigchld] in
  let restore () = ignore (Unix.sigprocmask Unix.SIG_SETMASK old_mask) in
  let thunk ()   = match Unix.fork () with
    (* We should just clear the table, but we need wait and friends to know how
     * to handle a proc that's not in the table. *)
    | 0    -> Table.iter (fun proc -> proc.child <- false) table;
              None
    | pid  -> let proc = { pid = pid; status = None; child = true; } in
                Table.add table proc;
                Some proc in
    unwind_protect thunk restore

let spawn ?(quiet = false) thunk =
  match fork () with
  | Some proc -> proc
  | None      -> try thunk ();
                     exit 0
                 with e ->
                   if not quiet
                     then Printf.eprintf "Uncaught exception: %s\n"
                                         (Printexc.to_string e);
                   exit 2

(*
 * The SIGCHLD handler does non-hanging waits as many times as
 * necessary, stashing each result in the table, and then resets itself.
 *)
let rec handle_sigchld num =
  try while true do
    wait_and_save [Unix.WNOHANG]
  done with
  | Unix.Unix_error (Unix.ECHILD, "waitpid", _)
  | Not_found ->
      Sys.set_signal num (Sys.Signal_handle handle_sigchld)

let autoreap ()       = handle_sigchld Sys.sigchld
let don't_autoreap () = Sys.set_signal Sys.sigchld Sys.Signal_default
      
let rec wait proc =
  if not proc.child then raise Not_child;
  match proc.status with
  | Some status -> status
  | None        ->
    (try wait_and_save ~pid:proc.pid [] with
     | Unix.Unix_error (Unix.ECHILD, _, _) ->
       if proc.status = None
       then raise Not_found);
    wait proc

let status_of_proc proc =
  if not proc.child then raise Not_child;
  (try wait_and_save ~pid:proc.pid [Unix.WNOHANG] with
   | Unix.Unix_error (Unix.ECHILD, "waitpid", _) | Not_found -> ());
  proc.status

let is_child proc    = proc.child
let pid_of_proc proc = proc.pid

let wait_any procs =
    if procs = [] || List.exists (not % is_child) procs
      then raise Not_child;
    let old_mask = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigchld] in
    unwind_protect
      (fun _ ->
        while_none
        (fun _ -> find' (fun proc -> proc.status <> None) procs)
          (fun _ -> try
              let pid, status = Unix.wait () in
                 stash_status pid status
             with Unix.Unix_error (Unix.EINTR, _, _) -> ()
          )
      )
      (fun _ -> Unix.sigprocmask Unix.SIG_SETMASK old_mask)

(* This is kind of complicated.  First, we look up the list of any procs
 * we already know about.  Then, if and ONLY if all the procs we know
 * about have terminated, we search for any running processes that we
 * don't know about.  First, we try waiting on it -- if these succeeds,
 * then we know it's a child, and we add it to the table.  (We store the
 * status if it's exited, but note that this case can happen only if
 * autoreaping is off; if it's on, the process will be reaped already.)
 * If it's not a child, we check if it exists at all, and if so, we
 * create a non-child Proc.t to represent it.  If we succeed in finding
 * an unknown process either way, it becomes known and is added to the
 * returned list.
 *)
let procs_of_pid pid =
  if pid < 1 then raise (Invalid_argument "procs_of_pid");
  let package_up res =
    let proc = match res with
      | -1, _ -> { pid = pid; status = None;   child = false; }
      | 0,  _ -> { pid = pid; status = None;   child = true; }
      | _,  s -> { pid = pid; status = Some s; child = true; } in
    Table.add table proc;
    proc in
  let possibilities = find_pid pid in
    if List.exists (fun proc -> proc.status = None) possibilities
    then possibilities
    else
      try package_up (Unix.waitpid [Unix.WNOHANG] pid) :: possibilities
      with Unix.Unix_error (Unix.ECHILD, "waitpid", _) ->
        try Unix.kill pid 0;
            package_up (-1, Unix.WEXITED 0) :: possibilities
        with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), "kill", _)
          -> possibilities

let proc_of_pid pid = match procs_of_pid pid with
  | []       -> raise Not_found
  | proc::__ -> proc

let kill ?(raise = true) signal proc =
  try Unix.kill proc.pid signal with
  | Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), "kill", _)
      when not raise -> ()

let exit_with_status = function
  | WEXITED n ->
      exit n
  | WSIGNALED n ->
      Sys.set_signal n Sys.Signal_default;
      Unix.kill n (Unix.getpid ());
      exit (-1)
  | _ ->
      exit 0

type execspec = { path:    bool option;
                  program: string;
                  argv0:   string option;
                  args:    string list; }

let with_execspec s kont =
  kont ?path:s.path s.program ?argv0:s.argv0 s.args

let execspec ?path program ?argv0 args =
  { path    = path;
    program = program;
    argv0   = argv0;
    args    = args; }

let exec_program ?(path = true) prog ?(argv0 = prog) args =
  (if path then Unix.execvp else Unix.execv) prog
    (Array.of_list (argv0 :: args))

let exec cmd = exec_program ~path:false "/bin/sh" ["-c"; cmd]

let vfork_program ?path prog ?argv0 args =
  IVar.with_interprocess_protect @@ fun protect ->
    spawn (fun _ -> protect (fun _ -> exec_program ?path prog ?argv0 args))

let vfork cmd = vfork_program ~path:false "/bin/sh" ["-c"; cmd]

let system_program ?path prog ?argv0 args =
  wait (vfork_program ?path prog ?argv0 args)

let system cmd = wait (vfork cmd)

(* Autoreaping is on be default. *)
let _ = autoreap ()
