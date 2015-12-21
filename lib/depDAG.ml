open Util

module Q = PriorityQueue

type task = unit -> Proc.t

(* We build a process DAG by specifying each process to run
 * along with its dependencies.  Sharing works. *)
type t = {
  goal:   task;
  prio:   int;
  deps:   t list;
  temp:   (int ref * (status list) ref) option ref;
}
(* Temporary data for assembly the priority structure. *)
and status = {
  s_goal : task;
  s_prio : int;
  s_next : status list ref;
  s_wait : int ref;
}

let make ?(prio = 0) goal deps = {
  goal  = goal;
  prio  = prio;
  deps  = deps;
  temp  = ref None;
}

let make_par ?(prio = 0) deps = {
  goal  = (fun _ -> Proc.spawn (fun _ -> ()));
  prio  = prio;
  deps  = deps;
  temp  = ref None;
}

(* We'll manage our state with a priority queue.  Function [prepare]
 * adds ready-to-go processes to the queue and prepares all processes
 * with their reference information.
 *
 * This code is not reentrant.
 *)
let prepare =
  (* Helper function to build the priority queue, and allocate the
   * refs and connections. *)
  let rec build next queue = function
   (* Seen nodes *)
   | { temp = { contents = Some (my_wait, my_next) } } as n
       -> my_next := next @ !my_next;
          let me = { s_goal = n.goal;  s_prio = n.prio;
                     s_next = my_next; s_wait = my_wait; } in
          List.fold_left (build [me]) queue n.deps
   (* Not yet seen nodes *)
   | n -> let my_wait = ref (List.length n.deps) in
          let my_next = ref [] in
          let queue   = match n.deps with
                        | [] -> Q.insert n.prio (n.goal, my_next) queue
                        | _  -> queue in
          n.temp := Some (my_wait, my_next);
          build next queue n in

  (* Helper function to clear the cached data from running build. *)
  let rec clear n = n.temp := None; List.iter clear n.deps in

  fun dag ->
    let it = build [] Q.empty dag in
    clear dag; it

let run =
  let add_if_ready queue next =
    decr next.s_wait;
    if !(next.s_wait) = 0
      then Q.insert next.s_prio (next.s_goal, next.s_next) queue
      else queue in

  let rec loop n running queue =
    match running with
    | [] when Q.is_empty queue -> ()
    | _  when Q.is_empty queue || n = 0 ->
        let pid     = Proc.wait_any (List.map fst running) in
        let queue   = List.fold_left add_if_ready queue
                                     (List.assq pid running) in
        let running = List.remove_assq pid running in
          loop (n + 1) running queue
    | _ ->
        let (goal, next) = Q.peek queue in
        let queue        = Q.remove_min queue in
        let pid          = goal () in
          loop (n - 1) ((pid, !next) :: running) queue in

  fun ?(n = 1) dag -> loop n [] (prepare dag)

(*
let test str = {|
  Proc.spawn {|
    print_int (Unix.getpid ());
    print_string ": ";
    print_endline str;
    ignore (input_line stdin)
  |}
|}

let a = make (test "A") [ ]
let b1 = make (test "B1") [ a ]
let b2 = make (test "B2") [ a ]
let c = make (test "C") [ ]
let d = make (test "D") [ b1; b2; c ]

let dag = d
;;
run 3 dag
*)
