(* vim: set ft=ocaml : *)
(** Base module for shtreams, an abstraction of producers of typed data.
 *
 * Shtreams are modeled on the standard library's [Stream] module, but
 * provide additional features.  The main difference is that a shtream
 * is easily converted to and from another kind of data source: an
 * [in_channel].  A shtream may be constructed from a channel (or a data
 * source such as a command to run) by providing a reader to extract
 * shtream elements from the [in_channel].  When converting a shtream
 * back to a channel with {!Shtream.channel_of}, if the shtream was
 * constructed from a channel in the first place, that channel is
 * returned, but if some of the shtream construction is happening
 * internally, a child process is spawned to compute the shtream
 * asynchronously.
 *
 * Operations in this module are all
 * indifferent to the type of elements in the {!Shtream.t}.
 * See modules {!AnyShtream}, {!LineShtream}, and {!StringShtream} for
 * operations specialized on the element type.
 * Most shtream operations are common to all Shtream modules; these
 * may also be found in {!Shtream.COMMON}.
 *)

(** Raised on attempts to retrieve elements from an empty
 * {!t}.  Callbacks such as the argument to {!from}
 * or {!map} may raise this to indicate the end of the
 * shtream they are producing. *)
exception Failure

(** A shtream with elements of type ['a] *)
type 'a t

(** Raised if a coshtream is no longer accepting data.  This could
 * happen, for example, were the shtream-consuming function to
 * return before exhausting the shtream. *)
exception CoFailure

(** A coshtream accepting elements of type ['a] *)
type 'a co_t

include ShtreamSig.S
  with type 'a t := 'a t
   and type 'a co_t := 'a co_t

(** {1 Reading and Writing Shtreams} *)

(** Make an [in_channel] from a {!Shtream.t}.
 * [Shtream.channel_of writer s] returns an [in_channel] whose contents
 * correspond to the shtream [s].  If [s] is already a channel, it may
 * return that channel (or a copy).
 *
 * However, if [s] is internal (made
 * with {!Shtream.from} or involves internal processing, it forks a
 * child process with a pipe from its [stdout].  The child process calls
 * [writer] for each element of the shtream, and [writer] must print
 * that element to [stdout].  If [?before] or [?after] is given, those
 * will be called before (or after) iterating the shtream in the child
 * process.  If a process if forked and [?procref] is given, its
 * {!Proc.t} is saved in the [ref].
 *)
val channel_of  : ?procref:Channel.procref ->
                  ?before:(unit -> unit) -> ?after:(unit -> unit) ->
                  ('a -> unit) -> 'a t -> in_channel

(**
 * Construct a {!Shtream.t} from a reader and an [in_channel].
 * The reader should raise [End_of_file] to indicate the end of the
 * channel.  It can also use
 * {!shtream_errors}.
 * The channel is dup'd for the shtream, which
 * means that closing or duping onto it has no effect on the shtream.
 *
 * (The optional argument [?hint] (default [None]) is for internal use.
 * It indicates that this reader was a default supplied by another
 * function in the system rather than chosen by the reader, and thus
 * the system remains free to select a different record reader.)
 *)
val of_channel  : ?hint:(Reader.raw_line -> 'a) ->
                  (in_channel -> 'a) -> in_channel -> 'a t

(**
 * Construct a {!Shtream.t} from a reader and a filename.  The arguments
 * are as for {!of_channel}.
 *)
val of_file     : ?hint:(Reader.raw_line -> 'a) ->
                  (in_channel -> 'a) -> string -> 'a t

(**
 * Construct a {!Shtream.t} from a reader and an external command.
 * If [?procref] is given, stash the child {!Proc.t}; if [?dups] is
 * given, perform the dups in the child process.
 * The remaining arguments are as for {!of_channel}.
 *)
val of_command  : ?procref:Channel.procref ->
                  ?dups:Channel.dup_spec ->
                  ?hint:(Reader.raw_line -> 'a) ->
                  (in_channel -> 'a) -> string -> 'a t

(**
 * Construct a {!Shtream.t} from a reader and an external program.
 * If [?procref] is given, stash the child {!Proc.t}; if [?dups] is
 * given, perform the dups in the child process.
 * The remaining arguments are as for {!of_channel}.
 *)
val of_program  : ?procref:Channel.procref ->
                  ?dups:Channel.dup_spec ->
                  ?hint:(Reader.raw_line -> 'a) ->
                  (in_channel -> 'a) ->
                  ?path:bool -> string -> ?argv0:string -> string list ->
                  'a t

(**
 * Construct a {!Shtream.t} from a reader and a thunk.
 * Spawns a child and calls the thunk in the child process.
 * If [?procref] is given, stash the child {!Proc.t}; if [?dups] is
 * given, perform the dups in the child process.
 * The remaining arguments are as for {!of_channel}.
 *)
val of_thunk   : ?procref:Channel.procref ->
                 ?dups:Channel.dup_spec ->
                 ?hint:(Reader.raw_line -> 'a) ->
                 (in_channel -> 'a) -> (unit -> unit) -> 'a t

(** The signature for operations common to all shtream modules.
 * This includes the majority of shtream operations.  What remains here
 * in {!Shtream} are functions that do input or output, and thus require
 * reading or writing advice.
 *)
module type COMMON = sig
  exception Failure
  (** Alias for {!Shtream.Failure} *)
  exception CoFailure
  (** Alias for {!Shtream.CoFailure} *)
  include ShtreamSig.S
end
