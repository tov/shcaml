(** 
 * Generalized channels and file descriptor manipulation.
 *)

(**
 * {2 Types}
 *)

(**
 * {3 Channel Types}
 *)

(** The abstract type for UNIX file descriptors. *)
type descr            = Unix.file_descr

(** Union of [in_channel] and [out_channel].  Returned by {!open_thunk},
 * {!open_command}, and {!open_program}. *)
type any_channel      = [ `InChannel  of in_channel
                        | `OutChannel of out_channel ]

(** Generalized input channels.  Union of buffered [in_channel] and
 * UNIX file descriptors, both abstract and as [int]s. *)
type gen_in_channel   = [ `InChannel  of in_channel
                        | `InDescr    of descr
                        | `InFd       of int ]

(** Generalized output channels.  Union of buffered [out_channel] and
 * UNIX file descriptors, both abstract and as [int]s. *)
type gen_out_channel  = [ `OutChannel of out_channel
                        | `OutDescr   of descr
                        | `OutFd      of int ]

(** Generalized channels. Union of {!gen_in_channel} and
 * {!gen_out_channel}. *)
type gen_channel      = [ gen_in_channel
                        | gen_out_channel ]

(**
 * {3 Dup Source Types}
 *)

(** Sources for input dup operations.
 * - [Channel.dup2 (`Filename s, gen)] opens the file [s] on
 *   generalized channel [gen].
 * - [Channel.dup2 (`Close, gen)] closes [gen].
 * - [Channel.dup2 (`Null, gen)] opens [/dev/null] for input
 *   on [gen].
 *)
type dup_in_source    = [ gen_in_channel
                        | `Filename   of string
                        | `Close
                        | `Null ]

(** Sources for output dup operations.
 * - [`Filespec] allows specifying how an output file is to be
 * opened according to {!clobber_spec}.
 * - The other constructors are as for
 * {!dup_in_source}, except that [`Filename] uses {!Channel.clobber}
 * as its open mode.
 *)
type dup_out_source   = [ gen_out_channel
                        | `Filename   of string
                        | `Close
                        | `Null
                        | `Filespec   of string * clobber_spec ]

(** File open modes for {!dup_out_source}.
 * - [`Clobber] opens the file and truncates it if it exists.
 * - [`NoClobber] fails if the file exists.
 * - [`Append] opens the file for appending and creates it if it doesn't
 * exist.
 * - [`AppendOnly] opens the file for appending if it exists and fails
 * otherwise.
 *)
and clobber_spec      = [ `Clobber | `NoClobber | `Append | `AppendOnly ]

(** Union of input and output dup sources. *)
type dup_source       = [ dup_in_source   | dup_out_source ]

(**
 * {3 Common Argument Types}
 *)

type dup_in_spec      = (dup_in_source * gen_in_channel) list
(** A list of dups on input channels. *)
type dup_out_spec     = (dup_out_source * gen_out_channel) list
(** A list of dups on output channels. *)
type dup_spec         = (dup_source * gen_channel) list
(** A list of dups (either direction). *)

type pipe_spec        = gen_channel list
(** A list of channels to connect to pipes. *)

type procref          = Proc.t option ref
(** A cell in which to stash a {!Proc.t}. Used as an out-parameter
 *  by functions that fork. *)

(*
 * {2 Values}
 *)

(**
 * {3 Opening and Closing}
 *)

(**
 * The default {!clobber_spec} for when {!dup2} opens a file for output.
 *)
val clobber          : clobber_spec ref

(**
 * Get the {!descr} underlying a generalized channel.  Note that if the
 * channel is a managed [in_channel] or [out_channel], the descriptor
 * may be closed when the channel is collected.
 *)
val descr_of_gen     : gen_channel -> descr

(** The abstract file descriptor of a concrete integer. *)
val descr_of_fd      : int -> descr

(** The concrete integer of an abstract file descriptor. *)
val fd_of_descr      : descr -> int


(** Open a file for input.  Attaches a finalizer to close the channel. *)
val open_file_in     : string -> in_channel

(** Open a file for output.  Attaches a finalizer to close the channel. *)
val open_file_out    : string -> out_channel


(** Open a [/dev/null] for input.  Attaches a finalizer. *)
val null_in          : unit -> in_channel

(** Open a [/dev/null] for output.  Attaches a finalizer. *)
val null_out         : unit -> out_channel


(** Close an input channel. *)
val close_in         : in_channel -> unit

(** Close an output channel. *)
val close_out        : out_channel -> unit

(** Close a generalized channel. *)
val close_gen        : gen_channel -> unit

(**
 * Dup and Friends
 *)

(** Copy an underlying file descriptor.  [Channel.dup2 (src, dest)]
 * copies [src] to [dest] so that future operations on [dest] use [src].
 * If [dest] is an [out_channel], it is flushed first; if [dest] in an
 * [in_channel], its buffer is {b discarded}.
 *
 * Some sources have special behaviors; see {!dup_in_source} and
 * {!dup_out_source}.
 * *)
val dup2             : dup_source * gen_channel -> unit

(** Move an underlying file descriptor.  This is like {!dup2}, but the
 * source channel is closed after the operation (unless the source and
 * the destination are the same).
 *)
val mov2             : dup_source * gen_channel -> unit

(**
 * Perform redirections before a thunk and reverse them afterward.
 *)
val with_dups        : dup_spec -> (unit -> 'a) -> 'a

(**
 * Duplicate an [in_channel] from an input dup source.
 *)
val dup_in           : dup_in_source -> in_channel

(**
 * Duplicate an [out_channel] from an input dup source.
 *)
val dup_out          : dup_out_source -> out_channel

(**
 * {3 Connecting to Other Processes}
 *)

(**
 * Spawn a thunk, opening pipes.  [Channel.open_thunk ~pipes ~dups
 * thunk] forks and calls [thunk] in the child process.  Any given
 * dups are performed in the child before calling the thunk.  Channels
 * specified in [pipes] are connected to pipes, and the other ends are
 * returned in a list, along with the {!Proc.t} of the child process.
 *
 * The functions {!open_thunk_in}, {!open_thunk_out}, {!open_thunk2},
 * and {!open_thunk3} are special cases.  Rather than return the
 * {!Proc.t}, they stash it in the optional argument [?procref].
 *)
val open_thunk  : ?pipes:pipe_spec ->
                  ?dups:dup_spec ->
                  (unit -> unit) -> Proc.t * any_channel list

val open_thunk_in  : ?procref:procref ->
                     ?dups:dup_spec ->
                     (unit -> unit) -> in_channel
(** Spawn a thunk and pipe from its [stdout]. *) 
val open_thunk_out : ?procref:procref ->
                     ?dups:dup_spec ->
                     (unit -> unit) -> out_channel
(** Spawn a thunk and pipe to its [stdin]. *) 
val open_thunk2    : ?procref:procref ->
                     ?dups:dup_spec ->
                     (unit -> unit) -> in_channel * in_channel
(** Spawn a thunk and pipe from its [stdout] and [stderr]. *) 
val open_thunk3    : ?procref:procref ->
                     ?dups:dup_spec ->
                     (unit -> unit) -> out_channel * in_channel * in_channel
(** Spawn a thunk and pipe its [stdin], [stdout], and [stderr]. *) 

(**
 * Spawn a command, opening pipes.  Like {!open_thunk}, but takes a
 * command to run in the shell.
 *)
val open_command : ?pipes:pipe_spec ->
                   ?dups:dup_spec ->
                   string -> Proc.t * any_channel list

val open_command_in  : ?procref:procref ->
                       ?dups:dup_spec ->
                       string -> in_channel
(** Spawn a command and pipe from its [stdout]. *) 
val open_command_out : ?procref:procref ->
                       ?dups:dup_spec ->
                       string -> out_channel
(** Spawn a command and pipe to its [stdin]. *) 
val open_command2    : ?procref:procref ->
                       ?dups:dup_spec ->
                       string -> in_channel * in_channel
(** Spawn a command and pipe from its [stdout] and [stderr]. *) 
val open_command3    : ?procref:procref ->
                       ?dups:dup_spec ->
                       string -> out_channel * in_channel * in_channel
(** Spawn a command and pipe its [stdin], [stdout], and [stderr]. *) 

(**
 * Spawn a program with arguments, opening pipes.
 * Like {!open_thunk}, but takes a program and arguments.
 *)
val open_program : ?pipes:pipe_spec ->
                   ?dups:dup_spec ->
                   ?path:bool -> string -> ?argv0:string -> string list ->
                   Proc.t * any_channel list

val open_program_in  : ?procref:procref ->
                       ?dups:dup_spec ->
                       ?path:bool -> string -> ?argv0:string -> string list ->
                       in_channel
(** Spawn a program and pipe from its [stdout]. *) 
val open_program_out : ?procref:procref ->
                       ?dups:dup_spec ->
                       ?path:bool -> string -> ?argv0:string -> string list ->
                       out_channel
(** Spawn a program and pipe to its [stdin]. *) 
val open_program2    : ?procref:procref ->
                       ?dups:dup_spec ->
                       ?path:bool -> string -> ?argv0:string -> string list ->
                       in_channel * in_channel
(** Spawn a program and pipe from its [stdout] and [stderr]. *) 
val open_program3    : ?procref:procref ->
                       ?dups:dup_spec ->
                       ?path:bool -> string -> ?argv0:string -> string list ->
                       out_channel * in_channel * in_channel
(** Spawn a program and pipe its [stdin], [stdout], and [stderr]. *) 

(**
 * {3 Doing Things with Strings}
 *)

val string_of_channel : in_channel -> string
(** Read the entire contents a channel into a string. *)
val string_of_command : ?procref:procref -> string -> string
(** Collect the output of a command as a string. *)
val string_of_program : ?procref:procref -> ?path:bool -> string ->
                        ?argv0:string -> string list -> string
(** Collect the output of a program as a string. *)

val open_string_in    : string -> in_channel
(** Open an [in_channel] whose contents is a given string. *)
val with_out_string   : (out_channel -> 'a) -> 'a * string
(** Collect the output of a thunk in a string.  Given a thunk, collects
 * everything it prints to [stdout] and returns the return value of the
 * thunk and the collected output. *)

(**
 * {3 Directories}
 *
 * These functions correspond directly to those of the same name in the
 * [Unix] structure.  However, these directory handles are managed by
 * the garbage collected and thus closed if they become unreachable.
 *)

type directory
(** A managed dir handle. *)

val opendir   : string -> directory
(** Open a managed dir handle.  See [Unix.opendir]. *)
val closedir  : directory -> unit
(** Manually close a managed dir handle.  See [Unix.closedir]. *)
val readdir   : directory -> string
(** Read an entry from a managed dir handle.  See [Unix.readdir]. *)
val rewinddir : directory -> unit
(** Rewind a managed dir handle. See [Unix.rewinddir]. *)


(** Convenience operators for specifying shell-style dups.
 *
 * This module provides one prefix operator and a surfeit of infix
 * operators.  The prefix operator {!(!%)} reveals the integer value of
 * an abstract UNIX file descriptor.  The infix operators are for
 * specifying dups, as follows:
 *
 * - Each operator dups its right argument onto its left.  This is the
 *   {i reverse} of {!dup2} and corresponds to the syntax of the Bourne
 *   shell.
 * - The [<] or [>] indicates the direction of IO: [<] is for input
 *   channels and [>] for output channels.
 * - Each operator has a prefix sigil that indicates the type of its
 * first argument: {ul
 *  {- [*] is for {!gen_in_channel}s and {!gen_out_channel}s}
 *  {- [%] is for integer file descriptors}
 *  {- [/] is for [in_channel]s and [out_channel]s}
 *  {- Use [(!%)] on {!descr} to get an [int] suitable for the [%]
 *  operations.)}
 * }
 * - Some operators have a suffix sigil that indicates the type
 * of their second arguments: {ul
 *  {- [&] means the second argument is the same type as the first}
 *  {- [*], [%], [/] are as for the first argument}
 * }
 * - Operators that dup from files indicate the mode in which to open
 * the file: {ul
 *  {- [<] opens a file for reading}
 *  {- [>] opens a file for writing, using the mode from {!clobber}}
 *  {- [>!] opens for writing, always clobbering ([`Clobber])}
 *  {- [>?] opens for writing, never clobbering ([`NoClobber])}
 *  {- [>>] opens for appending ([`Append])}
 *  {- [>>!] opens for appending but won't create ([`AppendOnly])}
 * }
 *
 * Some example dups:
 * - [ 2 %>& 1 ] redirects error output onto standard output
 * - [ 0 %< "file" ] redirects input to come from the file
 * - [ 1 %>/ c ], where [c : out_channel], redirects standard output
 *   into the channel
 * - [ c /<* `Close ] closes [c : in_channel]
 *)
module Dup : sig
  type 'a dup_in_arg  = 'a
    constraint 'a = [> dup_in_source ] * [> gen_in_channel ]
  (** An argument to dup acting on input channels *)
  type 'a dup_out_arg = 'a
    constraint 'a = [> dup_out_source ] * [> gen_out_channel ]
  (** An argument to dup acting on output channels *)

  val ( !% )    : descr -> int
  (** Get the integer value of an abstract file descriptor. *)

  val ( *<& )   : gen_in_channel   -> dup_in_source    -> 'a dup_in_arg
  (** Dup a {!Channel.dup_in_source} onto a {!Channel.gen_in_channel}. *)
  val ( *< )    : gen_in_channel   -> string           -> 'a dup_in_arg
  (** Open a file for reading on a {!Channel.gen_in_channel}. *)
  val ( *>& )   : gen_out_channel  -> dup_out_source   -> 'a dup_out_arg
  (** Dup a {!Channel.dup_out_source} onto a {!Channel.gen_out_channel}. *)
  val ( *> )    : gen_out_channel  -> string           -> 'a dup_out_arg
  (** Open a file for writing on a {!Channel.gen_out_channel}. *)
  val ( *>! )   : gen_out_channel  -> string           -> 'a dup_out_arg
  (** Open a file on a {!Channel.gen_out_channel}, clobbering. *)
  val ( *>? )   : gen_out_channel  -> string           -> 'a dup_out_arg
  (** Open a file on a {!Channel.gen_out_channel}, without clobbering. *)
  val ( *>> )   : gen_out_channel  -> string           -> 'a dup_out_arg
  (** Open a file on a {!Channel.gen_out_channel}, appending. *)
  val ( *>>! )  : gen_out_channel  -> string           -> 'a dup_out_arg
  (** Open a file on a {!Channel.gen_out_channel}, appending, without
   * creating. *)

  val ( %<& )   : int              -> int              -> 'a dup_in_arg
  (** Dup a file descriptor from another file descriptor. *)
  val ( %< )    : int              -> string           -> 'a dup_in_arg
  (** Open a file for reading on a file descriptor. *)
  val ( %>& )   : int              -> int              -> 'a dup_out_arg
  (** Dup a file descriptor from another file descriptor. *)
  val ( %> )    : int              -> string           -> 'a dup_out_arg
  (** Open a file for writing on a file descriptor. *)
  val ( %>! )   : int              -> string           -> 'a dup_out_arg
  (** Open a file on a file descriptor, clobbering. *)
  val ( %>? )   : int              -> string           -> 'a dup_out_arg
  (** Open a file on a file descriptor, without clobbering. *)
  val ( %>> )   : int              -> string           -> 'a dup_out_arg
  (** Open a file on a file descriptor, appending. *)
  val ( %>>! )  : int              -> string           -> 'a dup_out_arg
  (** Open a file on a file descriptor, appending, without creating. *)

  val ( /<& )   : in_channel       -> in_channel       -> 'a dup_in_arg
  (** Dup an [in_channel] from another [in_channel]. *)
  val ( /< )    : in_channel       -> string           -> 'a dup_in_arg
  (** Open a file for reading on an [in_channel]. *)
  val ( />& )   : out_channel      -> out_channel      -> 'a dup_out_arg
  (** Dup an [out_channel] from another [out_channel]. *)
  val ( /> )    : out_channel      -> string           -> 'a dup_out_arg
  (** Open a file for writing on an [out_channel]. *)
  val ( />! )   : out_channel      -> string           -> 'a dup_out_arg
  (** Open a file on an [out_channel], clobbering. *)
  val ( />? )   : out_channel      -> string           -> 'a dup_out_arg
  (** Open a file on an [out_channel], without clobbering. *)
  val ( />> )   : out_channel      -> string           -> 'a dup_out_arg
  (** Open a file on an [out_channel], appending. *)
  val ( />>! )  : out_channel      -> string           -> 'a dup_out_arg
  (** Open a file on an [out_channel], appending, without
   * creating. *)

  val ( *>% )   : gen_out_channel  -> int              -> 'a dup_out_arg
  (** Dup a {!Channel.gen_out_channel} from a file descriptor. *)
  val ( *>/ )   : gen_out_channel  -> out_channel      -> 'a dup_out_arg
  (** Dup a {!Channel.gen_out_channel} from an [out_channel]. *)
  val ( %>* )   : int              -> dup_out_source   -> 'a dup_out_arg
  (** Dup a file descriptor from a {!Channel.dup_out_source}. *)
  val ( %>/ )   : int              -> out_channel      -> 'a dup_out_arg
  (** Dup a file descriptor from an [out_channel]. *)
  val ( />* )   : out_channel      -> dup_out_source   -> 'a dup_out_arg
  (** Dup an [out_channel] from a {!Channel.dup_out_source}. *)
  val ( />% )   : out_channel      -> int              -> 'a dup_out_arg
  (** Dup an [out_channel] from a file descriptor. *)

  val ( *<% )   : gen_in_channel   -> int              -> 'a dup_in_arg
  (** Dup a {!Channel.gen_in_channel} from a file descriptor. *)
  val ( *</ )   : gen_in_channel   -> in_channel       -> 'a dup_in_arg
  (** Dup a {!Channel.gen_in_channel} from an [in_channel]. *)
  val ( %<* )   : int              -> dup_in_source    -> 'a dup_in_arg
  (** Dup a file descriptor from a {!Channel.dup_in_source}. *)
  val ( %</ )   : int              -> in_channel       -> 'a dup_in_arg
  (** Dup a file descriptor from an [in_channel]. *)
  val ( /<* )   : in_channel       -> dup_in_source    -> 'a dup_in_arg
  (** Dup an [in_channel] from a {!Channel.dup_in_source}. *)
  val ( /<% )   : in_channel       -> int              -> 'a dup_in_arg
  (** Dup an [in_channel] from a file descriptor. *)
end
