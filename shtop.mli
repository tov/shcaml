(** Utilities to support Shcaml in the toploop. *)

open Shcaml

val dir_shcaml          : unit -> unit
(** Toploop directive to set up the Shcaml toploop environment.
 * Installs Shcaml printers and opens several Shcaml modules. *)

val print_line          : Format.formatter -> 'a Line.t -> unit
(** Toploop printer for {!Line.t} *)
val print_descr         : Format.formatter -> Channel.descr -> unit
(** Toploop printer for {!Channel.descr} *)
val print_proc          : Format.formatter -> Proc.t -> unit
(** Toploop printer for {!Proc.t} *)
val print_in_channel    : Format.formatter -> in_channel -> unit
(** Toploop printer for [in_channel] *)
val print_out_channel   : Format.formatter -> out_channel -> unit
(** Toploop printer for [out_channel] *)
