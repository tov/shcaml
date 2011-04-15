(**
 * Purely-functional priority queues.
 *)

(** The abtract type of a priority queue with elements of type ['a]. *)
type +'a t

(** The empty priority queue. *)
val empty  : 'a t

(** Is a priority queue empty? *)
val is_empty : 'a t -> bool

(** Given a priority and an element, insert into a queue. *)
val insert : int -> 'a -> 'a t -> 'a t

(** Get the minimal value, if there is one. Raises [Not_found] if the
 * queue is empty. *)
val peek : 'a t -> 'a

(** Remove the minimal element and return the remaining queue.  Raises
 * [Not_found] if the queue is empty. *)
val remove_min : 'a t -> 'a t

