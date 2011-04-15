(** Hash tables with weak keys and strong values. *)

(** The output signature of the functor {!WeakPlus.Make}. *)
module type WEAKPLUS = sig
  type 'a t
  (** The type of weak-key hash table with values of type ['a] *)
  type key
  (** The key type of the hash table *)

  val create  : int -> 'a t
  (** Create a new, empty hash table with the given initial size.
      The table grows as needed. *)
  val resize  : ?size:int -> 'a t -> unit
  (** Resize a hash table.  If no [?size] is given, guesses based
      on the current size of the table. *)

  val add     : 'a t -> key -> 'a -> unit
  (** Bind a key to a value, replacing any previous value. *)
  val remove  : 'a t -> key -> unit
  (** Remove a key-value association. *)

  val find'   : 'a t -> key -> 'a option
  (** Look up a value by a key, returning [None] if not found. *)
  val find    : 'a t -> key -> 'a
  (** Look up a value by a key, raising [Not_found] if not found. *)
  val mem     : 'a t -> key -> bool
  (** Is a key present in the table? *)
  val count   : 'a t -> int
  (** How many keys are present in the table? *)
  val iter    : (key -> 'a -> unit) -> 'a t -> unit
  (** Apply a function to all bindings in a table. *)
end

(** Build an implementation of the weak-key hashtable structure.
    Takes the same argument structure as [Hashtbl.Make]. *)
module Make(H : Hashtbl.HashedType) : WEAKPLUS with type key = H.t
