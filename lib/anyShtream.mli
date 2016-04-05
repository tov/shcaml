(* vim: set ft=ocaml : *)
(**
 * Functor to create type-aware shtream modules.  The base shtream
 * module {!Shtream} is indifferent to the element type.  The functor
 * {!AnyShtream.Make}, on the other hand, produces a module with shtream
 * functions that know how read shtream from and write shtreams to
 * channels without a user-supplied reader or printer function.
 *
 * Modules {!LineShtream} and {!StringShtream} are both created using
 * this functor, though some values in {!LineShtream} are specialized
 * further.
 *)

(** Build a new shtream module.  The {!ELEM}
 * parameter {!E} specifies how to read and print shtream elements. *)
module Make(E : AnyShtreamSig.ELEM) : AnyShtreamSig.S with module Elem = E
