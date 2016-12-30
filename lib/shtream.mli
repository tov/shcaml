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

(** A shtream with elements of type ['a] *)
type 'a t

(** A coshtream accepting elements of type ['a] *)
type 'a co_t

(** The signature for operations common to all shtream modules. *)
module type COMMON = sig
  (** {1 Basic Shtream Operations} *)

  type 'a t
  type 'a co_t

  (** Raised on attempts to retrieve elements from an empty
   * {!t}.  Callbacks such as the argument to {!from}
   * or {!map} may raise this to indicate the end of the
   * shtream they are producing. *)
  exception Failure

  (** Raised if a coshtream is no longer accepting data.  This could
   * happen, for example, were the shtream-consuming function to
   * return before exhausting the shtream. *)
  exception CoFailure

  val from        : (int -> 'a option) -> 'a t
  (** Generate a shtream by repeatedly calling a function.
   * The function is called with the natural numbers starting from
   * 0, and should return
   * [Some v] to produce [v] or [None] to end the shtream.  The
   * function may also use {!shtream_errors}.
   * *)
  val close       : 'a t -> unit
  (** Free resources (such as channels) associated with a
   * shtream.  While channels will eventually be reclaimed by the
   * garbage collector, the file descriptor table could potentially
   * fill up without a collection.
  *)

  val of_list     : 'a list -> 'a t
  (** Construct a shtream from the elements of a list. *)
  val list_of     : 'a t -> 'a list
  (** Return the contents of a shtream as a list.  If the shtream is
   * infinite, this function won't terminate. *)
  val of_stream   : 'a Stream.t -> 'a t
  (** Convert a {i standard library} [Stream.t] to a shtream. *)
  val stream_of   : 'a t -> 'a Stream.t
  (** Convert a shtream to a {i standard library} [Stream.t]. *)

  val npeek       : ?n:int -> 'a t -> 'a list
  (** Get the first [n] (default [1]) elements of a shtream.
   * If there are fewer than [n] elements remaining, returns only
   * those.
   * Leaves the elements in the shtream. *)
  val peek        : ?n:int -> 'a t -> 'a option
  (** Get the [n]th (default [0]th) element of a shtream.  Returns
   * [Some v] if [v] is the [n]th zero-indexed element, and [None] if
   * the shtream has [n] or fewer elements remaining.  Leaves
   * elements in the shtream. *)
  val empty       : 'a t -> unit
  (** Return [()] if the shtream is empty.  Raises {!Failure} if
   * elements remain. *)
  val is_empty    : 'a t -> bool
  (** Is a shtream empty?  This may cause one element of the shtream
   * to be evaluted; for example, it may read an element from a
   * channel to check for [End_of_file].  The element is not
   * discarded. *)
  val status      : 'a t -> Proc.status option
  (** Retrieve the termination status of an exhausted shtream.  If the
   * shtream still has data, returns [None].  Otherwise, [Some
   * (Proc.WEXITED 0)] indicates normal termination and [Some n] for
   * non-zero [n] indicates abnormal termination.  If a shtream was made
   * from an external process (via {!Shtream.of_command}, for example),
   * then [n] is the exit code of the process.  (If the process closes its
   * output and continues running, the shtream will terminate with [Some
   * 0] rather than wait for the process.)
  *)

  val junk        : ?n:int -> 'a t -> unit
  (** Discard the first [n] (default 1) elements of a shtream.  If
   * fewer than [n] remain, discards them all. *)
  val next        : 'a t -> 'a
  (** Return and discard the next element of a shtream.  If the
   * shtream is empty, raises {!Failure}. *)
  val next'       : 'a t -> 'a option
  (** Return and discard the next element of a shtream.  If the
   * shtream is empty, returns [None]. *)

  (** {1 Shtream Construction and Observation} *)

  val iter        : ('a -> unit) -> 'a t -> unit
  (** Apply a function to each element of a shtream in turn.  This
   * function exhausts the shtream. *)
  val filter      : ('a -> bool) -> 'a t -> 'a t
  (** Lazily filter a shtream according to a predicate.
   * [Shtream.filter pred s] returns a new
   * shtream containing all the elements of [s] that satisfy [pred].
   * The order of the elements is preserved, and [s] becomes
   * invalid. *)
  val map         : ('a -> 'b) -> 'a t -> 'b t
  (** Lazily map a function over the elements of a shtream.  The old shtream
   * becomes invalid. *)
  val concat_map  : ('a -> 'b list) -> 'a t -> 'b t
  (** Lazily map a function over a shtream, concatenating the results.
   * [Shtream.concat_map f s] applies [f] to each element of [s] in
   * turn, concatenating the resulting lists to form a new shtream.
   * The old shtream [s] becomes invalid. *)
  val fold_left   : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Eagerly fold a function over a shtream.
   * [Shtream.fold_left f z s] applies the function [f] to each
   * element of [s] in turn, with an accumulating parameter
   * that starts at [z], and then returns the accumulated value.
   * Isomorphic to [List.fold_left]. *)
  val fold_right  : ('a -> 'b Lazy.t -> 'b) -> 'a t -> 'b -> 'b
  (** Lazily fold a function over a shtream.
   * [Shtream.fold_right f s z] applies [f] to each element
   * of [s] and a lazy value that, when forced, returns the
   * rest of the fold.  At the end of the shtream, the lazy value
   * returns [z].  Isomorphic to call-by-need [List.fold_right].
  *)

  val nil         : unit -> 'a t
  (** Return the empty shtream. *)
  val insert      : 'a -> 'a t -> unit
  (** Prepend an element to a shtream. *)
  val cons        : 'a -> 'a t -> 'a t
  (** Prepend an element to a shtream and return the shtream. *)
  val append      : 'a t -> 'a t -> 'a t
  (** Append two shtreams.  [Shtream.append s1 s2] returns a shtream
   * that first produces the elements of [s1], and should [s1] be
   * exhausted, then produces the elements of [s2].  This operation
   * invalidates both [s1] and [s2].
  *)

  (** {2:shtream_errors Shtream Error Handling }

   * Because a shtream may be used far from its creation site, the
   * client code of a shtream may be ill prepared to handle any
   * exceptions the shtream might raise.  To this end, we provide a
   * simple API for signaling and handling shtream errors.
   *
   * Here are several functions that a shtream generator (for
   * example, the callback given to {!from} or a reader) may call to
   * signal a condition:
   * *)

  val try_again   : unit   -> 'a
  (** Indicates that the generating function cannot produce a value
   * now.  The shtream evaluator should call it again. *)
  val warn        : ('a, unit, string, 'b) format4 -> 'a
  (** Indicates that the generating function has encountered a
   * problem.  The error handler will decide what to do. *)
  val fail_with   : Proc.status -> 'a
  (** Indicates that a shtream should terminate with the given error
   * code.  Calling [fail_with (Proc.WEXITED 0)] is equivalent to raising
   * {!Failure}.  Calling [fail_with st] results in an exit status of
   * [st].
  *)

  type error_handler = [`Warning of string | `Exception of exn] -> unit
  (** The type for a shtream error handler.  If a shtream generator
   * calls [warn s] then the error handler receives [`Warning s].  If
   * a shtream generator raises an exception [e], then the error_handler
   * receives [`Exception e].
  *)

  val current_error_handler  : error_handler ref
  (** Reference containing the shtream error handler.  This may be
   * changed to one of the predefined error handling functions such as
   * {!ignore_errors} or a user-defined function. *)

  val ignore_errors          : error_handler
  (** Ignore errors and continue evaluating the shtream. *)
  val warn_on_errors         : error_handler
  (** Print a warning on [stderr] and continue evaluating the shtream. *)
  val die_on_errors          : error_handler
  (** Print a warning on [stderr] terminate the shtream. *)
  val die_silently_on_errors : error_handler
  (** Just terminate the shtream. *)

  (** {1 Coshtreams}
   *
   * A shtream is a source of typed data; a coshtream, of course, is a
   * sink.  Given a function [f] that consumes a shtream, {!coshtream_of}
   * returns a coshtream handle that can be used to supply shtream
   * elements to [f] asynchronously.
   *
   * Because coshtreams work by calling [f] in a child process, they
   * must marshal shtream data over a pipe; thus, they work only if
   * the element type is serializable using [Marshal].  Moreover,
   * internal side effects in [f], such as mutating a ref cell, will not
   * be visible, since [f] is called in a separate process.
   * *)

  val coshtream_of : ?procref:Channel.procref -> ('a t -> 'b) -> 'a co_t
  (** Construct a coshtream from a shtream-consuming function.
   * [coshtream_of f] spawns a child process in which it invokes [f],
   * and relays values sent to the resulting coshtream into the
   * shtream given to [f].  If [procref] is provided, the child
   * {!Proc.t} is stashed therein.
  *)
  val conil        : unit -> 'a co_t
  (** Returns the empty coshtream.  The empty coshtream accepts
   * data ad infinitum. *)

  val conext       : 'a co_t -> 'a -> unit
  (** Send a value to a coshtream. *)
  val coclose      : 'a co_t -> unit
  (** Indicate the end of a coshtream.  The shtream consuming function
   * will see this as the end of the shtream. *)

  val annihilate   : 'a t -> 'a co_t -> unit
  (** Send the contents of a shtream to a coshtream.  This leaves
   * the coshtream open, and if the coshtream stops accepting data
   * before the shtream is exhausted, data {i may} remain in the shtream.
  *)

  (** {1 Low level (scary stuff)} *)

  (** Generate a shtream from a function, and include a close
   * operation.  This is like {!from}, except that the function
   * returns ['a] rather than ['a option], and must signal the end of
   * the shtream by raising {!Failure}.  If [close] is provided, it
   * will be called when the resulting shtream is closed; this could
   * be useful to free resources associated with the shtream.
  *)
  val from_low    : ?close:(unit -> unit) -> (int -> 'a) -> 'a t

  (** Invalidate a shtream and return a copy.  [claim s] returns
   * a shtream behaving identically to [s], while modifying [s] to
   * contain no more data.  This is useful for functions that want to
   * lay claim to a shtream argument while consuming it lazily.
  *)
  val claim       : 'a t -> 'a t

  (** Change the channel reader of a shtream.  If the given shtream is
   * a channel-based shtream (made with {!Shtream.of_channel}, for
   * example), this causes it to begin using the supplied reader to
   * produce shtream elements.
  *)
  val set_reader  : 'a t -> (in_channel -> 'a) -> unit

  (** Possibly change the channel reader of a shtream.  This is like
   * {!set_reader}, but it only changes the reader if the current
   * reader was defaulted rather than supplied by the user.  For
   * example, if a shtream is created with {!AnyShtream.S.of_channel}
   * and the user does not supply its option [?reader] argument, then
   * the reader may be changed by the system using {!hint_reader};
   * but if the user explictly specifies a reader, then {!hint_reader}
   * has no effect.
  *)
  val hint_reader : 'a t -> Reader.t -> unit

  type protector = Util.protector
  (** Alias for {!Util.protector} *)
  val add_protection : protector -> 'a t -> unit
  (** Add advice to adjust parameters while evaluating a shtream.  A
   * {!protector} is a function that, given a thunk, must return the value
   * of the thunk, but may perform some preparation first or cleanup
   * afterward.  Adding a protector to a shtream means that any internal
   * shtream evalution will be performed in the dynamic context of the
   * protector.  If more than one protector has been added to a shtream,
   * they will be performed with the newest protector on the outside.
  *)
  val add_cleanup : (unit -> unit) -> 'a t -> unit
  (** Add advice for closing a shtream.  This function adds a thunk to
   * be performed when a shtream is closed.  If more than one close action
   * has been added, they will be performed in order from oldest to
   * newest.
  *)
end

(** This includes the majority of shtream operations. *)
include COMMON
  with type 'a t := 'a t
   and type 'a co_t := 'a co_t

(** What remains here in {!Shtream} are functions that do input or
 *  output, and thus require reading or writing advice.
 *)

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
