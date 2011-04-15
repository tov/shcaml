(** Shtreams of {!Line.t}s.
 * This module is the result of applying {!AnyShtream.Make} to the
 * module {!LineElem}.
 * Thus, shtreams handled by this module are compatible with the shtreams
 * of {!Shtream}, {!StringShtream}, and modules created by
 * {!AnyShtream.Make}, but this module provides additional functions
 * for reading and writing ['a Line.t Shtream.t]s.
 *
 * This module specializes shtream-creating functions to store source
 * information in the resulting lines.
 *)

(** A line with source and sequence information.  This is the
 * {!AnyShtream.ELEM.initial} type for {!LineShtream}. *)
type sourced = <Line| seq: Line.present; source: Line.present >

(** The parameter given to {!AnyShtream.Make} to build this module. *)
module LineElem : AnyShtream.ELEM
  with type 'a elem = 'a Line.t
   and type initial = sourced

(** Most of the types and values in {!LineShtream} come from the
 * result of apply {!AnyShtream.Make}. *)
include AnyShtream.ANYSHTREAM with module Elem = LineElem

(** Construct a {!Line.t} reader from a record reader.  This is like
 * {!AnyShtream.ANYSHTREAM.elem_reader}, but allows specifying a
 * {!Line.source} to store in the lines. *)
val line_reader : ?source:Line.source -> Reader.t ->
                  (in_channel -> initial elem)

(** Annotate a shtream of lines with source and sequence information.
 * If the lines already have [seq] or [source] fields, these are
 * rewritten to reflect the given {!Line.source} and current sequence.
 *)
val annotate    : Line.source -> <| .. as 'a > Line.t t ->
                  <| seq :    Line.present;
                     source : Line.present;
                     .. as 'a > Line.t t
