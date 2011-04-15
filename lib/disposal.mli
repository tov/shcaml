(**
 * Registries for semi-automatic object disposal.
 * [Disposal.Make(D)] creates a registry for disposal of objects of
 * type [D.t].
 *
 * {!Disposal} ensures that for any managed object,
 * a disposer is called only once, whether manually or automatically,
 * {b except} for the default disposer [D.default],
 * which must be safe to call multiple times on any already-disposed
 * object.
 *)

(** The input signature of the functor {!Disposal.Make}. *)
module type DISPOSABLE = sig
  type t
  (** The type to dispose *)
  val equal : t -> t -> bool
  (** Identity predicate to associate objects with disposal actions. *)
  val hash : t -> int
  (** Hash function for managed objects. *)
  val default : t -> unit
  (** The default disposal action.  Must be safe to call multiple
      times on the same object. *)
end

(** The output signature of the functor {!Disposal.Make}. *)
module type DISPOSAL = sig
  (** The type of data being managed. *)
  type data

  (** Register a disposal function for a particular object.
      Returns the object. *)
  val register  : (data -> unit) -> data -> data

  (** Request that the garbage collector manage an object.  If no
      disposal action is given, uses [D.default].  When the object
      is collected, the disposal action will be run if the user hasn't
      called it manually first (or possibly again, if it's the default
      disposer). *)
  val manage    : ?disposer:(data -> unit) -> data -> data

  (** Manually dispose an object now.  If the object is not registered,
      uses D.default. *)
  val dispose   : data -> unit
end

(** Build a new disposal registry.  The {!DISPOSABLE} parameter [D]
    specifies how to dispose objects of type [D.t]. *)
module Make(D : DISPOSABLE) : DISPOSAL with type data = D.t
