(*
 * This module implements weak references and weak hash tables.  The
 * functor Make is used to construct a new weak hash table structure.
 *)
open Util

module type WEAKPLUS = sig
  type 'a t
  type key

  val create  : int -> 'a t
  val resize  : ?size:int -> 'a t -> unit
  val add     : 'a t -> key -> 'a -> unit
  val remove  : 'a t -> key -> unit
  val find'   : 'a t -> key -> 'a option
  val find    : 'a t -> key -> 'a
  val mem     : 'a t -> key -> bool
  val count   : 'a t -> int
  val iter    : (key -> 'a -> unit) -> 'a t -> unit
end

module Make(H : Hashtbl.HashedType) = struct
  type key = H.t

  (*
   * A weak hash table stores keys in an array of weak buckets.
   * We hash into the outer array, and then search the bucket to see if
   * the key has been retained.  Associated values are found in
   * an array of arrays of the same shape.
   *
   * We use an operation counter to determine when it might be a good
   * idea to resize the array.
   *)
  type 'a t = {
    mutable keys   : key Weak.t array;
    mutable values : 'a option array array;
    mutable ops    : int;
  }

  let minsize = 10

  (*
   * Some helper functions
   *)

  let hash h k  = (H.hash k) mod (Array.length h.keys)

  (* To find/set the buckets in which a particular key falls. *)
  let get_buckets h k =
    let index = hash h k in
      (h.keys.(index), h.values.(index))

  let put_buckets h k kb vb =
    let index = hash h k in
      h.keys.(index) <- kb;
      h.values.(index) <- vb

  (* To find the index of a key within a bucket. *)
  let get_index bucket k =
    let limit = Weak.length bucket in
    let rec loop i =
      if i < limit then
        match Weak.get bucket i with
         | Some k' when H.equal k k' -> Some i
         | _                         -> loop (i + 1)
        else None
    in loop 0

  (*
   * To add an association, we find the appropriate bucket and check
   * if the binding already exists -- if so, we replace it.  Otherwise,
   * we search for an open slot to place it in.  If none exists, we double
   * the bucket size, copy the contents, and insert into the new bucket.
   *)
  let real_add h k v =
    let update kb vb i =
      Weak.set kb i (Some k);
      vb.(i) <- Some v in
    let (key_bucket, val_bucket) = get_buckets h k in
    match get_index key_bucket k with
     | Some i -> val_bucket.(i) <- Some v
     | _      ->
       let limit = Weak.length key_bucket in
       let rec loop i =
         if i < limit then
           match Weak.get key_bucket i with
            | Some _ -> loop (i + 1)
            | _      -> update key_bucket val_bucket i
         else (
           let new_kb = Weak.create (2 * limit + 1) in
           let new_vb = Array.make (2 * limit + 1) None in
           for i = 0 to limit - 1 do
             match Weak.get key_bucket i with
              | Some _ as sk -> Weak.set new_kb i sk;
                                new_vb.(i) <- val_bucket.(i)
              | _            -> ()
           done;
           update new_kb new_vb limit;
           put_buckets h k new_kb new_vb) in
       loop 0

  (* Public methods *)

  let create n =
    let n = max n minsize in {
      ops    = 0;
      keys   = Array.make n (Weak.create 0);
      values = Array.make n (Array.create 0 None);
    }

  let count h =
    let result = ref 0 in
      for i = 0 to Array.length h.keys - 1 do
        let bucket = h.keys.(i) in
          for j = 0 to Weak.length bucket - 1 do
            match Weak.get bucket j with
             | Some _ -> result := !result + 1
             | _      -> ()
          done
      done;
      !result

  let iter f h =
    for i = 0 to Array.length h.keys - 1 do
      let kb = h.keys.(i) in
      let vb = h.values.(i) in
        for j = 0 to Weak.length kb - 1 do
          match Weak.get kb j, vb.(j) with
           | Some k, Some v -> f k v
           | _              -> ()
        done
    done

  let resize ?size h =
    let size = maybe size (fun () -> count h) id in
    let new_h = create size in
      iter (real_add new_h) h;
      h.keys   <- new_h.keys;
      h.values <- new_h.values;
      h.ops    <- new_h.ops

  let maybe_resize_helper h =
    let used = count h in
    let size = Array.length h.keys in
    if 4 * used < size or 4 * size < used
      then resize ~size:used h

  let tick_helper h =
    if h.ops >= 4 * Array.length h.keys then
      maybe_resize_helper h
    else
      h.ops <- h.ops + 1

  let add h k v =
    real_add h k v;
    tick_helper h

  let find' h k =
    let (key_bucket, val_bucket) = get_buckets h k in
    match get_index key_bucket k with
     | Some i -> val_bucket.(i)
     | None   -> None

  let find h k   = match find' h k with
   | Some v -> v
   | _      -> raise Not_found

  let mem h k    = find' h k <> None

  let remove h k =
    let (key_bucket, val_bucket) = get_buckets h k in
    match get_index key_bucket k with
     | Some i -> Weak.set key_bucket i None;
                 val_bucket.(i) <- None;
                 tick_helper h
     | _      -> ()
end

(** Weak reference cells. *)
module Ref : sig
  type 'a t
  (** A weak cell containing an ['a] *)
  val create : 'a -> 'a t
  (** Create a new weak cell containing a value. *)
  val get : 'a t -> 'a option
  (** Get the value in a weak cell, or [None] if it's been freed. *)
  val set : 'a t -> 'a -> unit
  (** Set the value of a weak cell. *)
  val clear : 'a t -> unit
  (** Clear the value from a weak cell. *)
end = struct
  type 'a t = 'a Weak.t
  let get w   = Weak.get w 0
  let set w v = Weak.set w 0 (Some v)
  let clear w = Weak.set w 0 None
  let create v =
    let w = Weak.create 1 in
      set w v;
      w
end
