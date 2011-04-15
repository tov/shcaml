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

(** {2 Line Fitting Interface} *)

(** {3 Types} *)

(** A fitting that consumes values of type ['a] and produces
 * values of type ['b].
 *)
type 'a t
  constraint 'a = 'b -> 'c

type 'a elem      = 'a LineShtream.elem
(** Alias for {!AnyShtream.ELEM.elem} at {!LineShtream}
 *
 * This is the type of elements that fittings know how to write to
 * external processes.
 *)
type initial      = LineShtream.initial
(** Alias for {!AnyShtream.ELEM.initial} at {!LineShtream}
 *
 * This is the parameter to the type {!elem} for specifying
 * the type of elements that fittings know how to read from
 * external processes.  That is, fittings constructed from external
 * processes produce values of type [initial elem].
 *)
type 'a shtream   = 'a Shtream.t
(** Alias for {!Shtream.t} *)
type 'a coshtream = 'a Shtream.co_t
(** Alias for {!Shtream.co_t} *)

#include "fitting.sig"

(** {2 Functorial Interface} *)

(** The input signature of the functor {!Fitting.Make}.
 *
 * This matches {!AnyShtream.ANYSHTREAM}, the output signature of
 * {!AnyShtream.Make}.
 * *)
module type SHTREAM = AnyShtream.ANYSHTREAM

(** The output signature of the functor {!Fitting.Make}. *)
module type FITTING = sig
  (** {3 Types} *)

  (** A fitting that consumes values of type ['a] and produces
   * values of type ['b].
   *)
  type 'a t
    constraint 'a = 'b -> 'c

  type 'a elem
  (**
   * This is the type of elements that fittings know how to write to
   * external processes.  This type comes from the functor
   * parameter {!SHTREAM}.
   *)
  type initial
  (** 
   * This is the parameter to the type {!elem} for specifying
   * the type of elements that fittings know how to read from
   * external processes.  That is, fittings constructed from external
   * processes produce values of type [initial elem].
   *
   * This type comes from the functor parameter {!SHTREAM}.
   *)
  type 'a shtream
  (** Alias for {!Shtream.t} *)
  type 'a coshtream
  (** Alias for {!Shtream.co_t} *)

  #include "fitting.sig"
end

(** Build a new fittings module.  The {!SHTREAM} parameter specifies the
 * underlying shtream implementation to use. *)
module Make(Shtream : SHTREAM) : FITTING
  with type initial      = Shtream.initial
   and type 'a elem      = 'a Shtream.elem
   and type 'a shtream   = 'a Shtream.t
   and type 'a coshtream = 'a Shtream.co_t
