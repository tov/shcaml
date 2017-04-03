(* vim: set ft=ocaml : *)
(**
 * Fittings represent processes, internal or external, that produce,
 * consume, or transform data.  This module provides basic
 * constructors to make fittings out of both external UNIX commands
 * and internal {!Shtream} functions, and fitting combinators
 * that combine fittings in a variety of ways.
 *
 * This module includes both functions specialized to shtreams of
 * {!Line.t} and a functor for creating a structure specialized for
 * sthreams of other types.
 *)

(** {1 Line Fitting Interface} *)

include FittingSig.S
  with type 'a elem = 'a LineShtream.elem
   and type initial = LineShtream.initial
   and type 'a shtream = 'a LineShtream.t
   and type 'a coshtream = 'a LineShtream.co_t

(** {1 Functorial Interface} *)

(** Build a new fittings module. The {!Shtream} parameter specifies the
    underlying shtream implementation to use. *)
module Make(Shtream : AnyShtream.S) : FittingSig.S
  with type initial      = Shtream.initial
   and type 'a elem      = 'a Shtream.elem
   and type 'a shtream   = 'a Shtream.t
   and type 'a coshtream = 'a Shtream.co_t
