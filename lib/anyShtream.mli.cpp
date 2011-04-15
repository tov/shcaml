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

(** The input signature of the functor {!AnyShtream.Make}. *)
module type ELEM = sig
(** The element type may be polymorphic, in which case the conversion
  * of elements to strings must handle any element.  The conversion
  * from strings (or reading from channels) is monomorphic, returning
  * shtream elements of a particular type.
  *)

  type 'a elem
  (** The element type for the resulting shtream module.  This type is
   * parameterized so that a shtream module might handle a family of
   * types.  The function {!string_of} needs handle ['a elem] for any
   * ['a]. *)
  type initial
  (** The parameter to {!elem} for values returned by conversions from
   * strings.  That is, [initial elem] is the type of shtream elements when
   * first read from a string or channel. *)

  val reader    : unit -> in_channel -> initial elem
  (** Make a reader of shtream elements.  The reader may be stateful;
   * a new one will be instantiated for each shtream. *)
  val of_string : unit -> string -> initial elem
  (** Make a parser of shtream elements.  The parser may be stateful;
   * a new one will be instantiated for each shtream. *)
  val string_of : unit -> 'a elem -> string
  (** Make a convertor of shtream elements to strings.  The resulting
   * function may be stateful; a new one will be instantiated for
   * shtream output operation. *)
end

(** The output signature of the functor {!AnyShtream.Make}.
 * The shtream and coshtream types in the resulting module are
 * compatible with other applications of the functor and with {!Shtream}.
 *
 * When {!AnyShtream.Make} is applied to a structure [Elem] (having
 * signature {!ELEM}), the resulting module knows how to write
 * shtreams of type ['a Elem.elem Shtream.t] and read shtreams of type
 * [Elem.initial Elem.elem Shtream.t].  Functions in the resulting
 * module take several optional parameters whose defaults are
 * supplied by [Elem]:
 * - [?(reader : in_channel -> initial elem)] defaults to
 *   [Elem.reader ()].
 * - [?(parse : string -> initial elem)] defaults to [Elem.of_string ()].
 * - [?(show : 'a elem -> string)] defaults to [Elem.string_of ()].
 *
 * This signature is equivalent to {!Fitting.SHTREAM},
 * the input signature of the functor {!Fitting.Make}.
 *)
module type ANYSHTREAM = sig
  #include "anyShtream.sig"
end

(** Build a new shtream module.  The {!ELEM}
 * parameter specifies how to read and print shtream elements. *)
module Make(E : ELEM) : ANYSHTREAM with module Elem = E
