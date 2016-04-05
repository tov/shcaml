(* vim: set ft=ocaml : *)

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
module type S = sig
  (** The result of {!AnyShtream.Make} contains all the type-indifferent
   * shtream operations from {!Shtream}. *)
  include Shtream.COMMON
    with type 'a t = 'a Shtream.t
     and type 'a co_t = 'a Shtream.co_t

  (** Access to the underlying element type and operations. *)
  module Elem : ELEM

  type 'a elem = 'a Elem.elem
  (** Alias for {!ELEM.elem} *)
  type initial = Elem.initial
  (** Alias for {!ELEM.initial} *)

  (** Construct an [initial elem] reader from a record reader.
   * Functions such as {!of_channel} and {!of_program} take a function
   * of the type returned here.
  *)
  val elem_reader : Reader.t -> (in_channel -> initial elem)

  (** Write the entire contents of a shtream on a channel.
   * For each element [x] of the shtream, it prints [init x], then
   * [show x], and then [term x] on the channel, and then flushes the
   * channel.
   * @param channel default = [stdout]
   * @param init default = [fun _ -> ""]
   * @param show default = [Elem.string_of ()]
   * @param term default = [fun _ -> "\n"]
  *)
  val output      : ?channel:out_channel ->
    ?init:('a elem -> string) ->
    ?term:('a elem -> string) ->
    ?show:('a elem -> string) ->
    'a elem t ->
    unit

  (** Construct an [in_channel] from the data in a
    * shtream.  If forking a child is necessary (see
    * {!Shtream.channel_of}), then the optional
    * parameter [?before] (resp. [?after]) is called in the child
    * before (resp. after) printing the shtream; anything printed on
    * [stdout] by [?before] ([?after]) appears in the resultant
    * [in_channel] before (after) the shtream data.
    *
    * The remaining arguments are as for {!output}.
  *)
  val channel_of  : ?procref:Channel.procref ->
    ?before:(unit -> unit) ->
    ?after:(unit -> unit) ->
    ?init:('a elem -> string) ->
    ?term:('a elem -> string) ->
    ?show:('a elem -> string) ->
    'a elem t -> in_channel

  (** Convert a shtream to a list of strings, using [?show]. *)
  val string_list_of      : ?show:('a elem -> string) ->
    'a elem t -> string list

  (** Convert a shtream to a {i standard library} [Stream.t] of
   * strings, using [?show]. *)
  val string_stream_of    : ?show:('a elem -> string) ->
    'a elem t -> string Stream.t

  (** Read a shtream from a channel, using [?reader]. *)
  val of_channel  : ?reader:(in_channel -> initial elem) ->
    in_channel -> initial elem t

  (** Read a shtream from a file, using [?reader]. *)
  val of_file     : ?reader:(in_channel -> initial elem) ->
    string -> initial elem t

  (** Read a shtream from the output of a command, using [?reader].
   * If [?procref] is given, stash the {!Proc.t}; if [?dups]
   * is given, perform the dups in the child process. *)
  val of_command  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    string ->
    initial elem t

  (** Read a shtream from the output of a process, using [?reader].
   * If [?procref] is given, stash the {!Proc.t}; if [?dups]
   * is given, perform the dups in the child process. *)
  val of_program  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    ?path:bool -> string -> ?argv0:string -> string list ->
    initial elem t

  (** Read a shtream from the output of a thunk, using [?reader].
   * If [?procref] is given, stash the {!Proc.t}; if [?dups]
   * is given, perform the dups in the child process. *)
  val of_thunk  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    (unit -> unit) ->
    initial elem t

  (** Construct a shtream from a list of strings, using [?parse]. *)
  val of_string_list   : ?parse:(string -> initial elem) ->
    string list -> initial elem t

  (** Construct a shtream from a {i standard
   * library} [Stream.t] of strings, using [?parse]. *)
  val of_string_stream : ?parse:(string -> initial elem) ->
    string Stream.t -> initial elem t
end
