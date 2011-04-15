open Util

module type DISPOSABLE = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val default : t -> unit
end

module type DISPOSAL = sig
  type data
  val register  : (data -> unit) -> data -> data
  val manage    : ?disposer:(data -> unit) -> data -> data
  val dispose   : data -> unit
end

module Make(D : DISPOSABLE) = struct
  module Table = WeakPlus.Make(struct
    include D
    type key    = t
    type data   = key -> unit
  end)

  type data = D.t

  let table = Table.create 32

  let dispose obj =
    try let disposer = Table.find table obj in
        Table.remove table obj;
        disposer obj
      with Not_found -> D.default obj

  let only_once f =
    let todo = ref true in
      fun x -> if !todo then (f x; todo := false)

  let raw_register disposer obj kont =
    if disposer == D.default then
      kont D.default
    else
      let disposer = only_once disposer in
        Table.add table obj disposer;
        kont disposer

  let register disposer obj =
    raw_register disposer obj ignore;
    obj

  let manage ?(disposer=D.default) obj =
    raw_register disposer obj (flip Gc.finalise obj);
    obj
end
