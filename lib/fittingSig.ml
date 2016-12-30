(* vim: set ft=ocaml : *)
(** Generic signature for Fittings *)

open Channel
open Channel.Dup

module type S = sig

  (** {1 Types} *)

  (** A fitting that consumes values of type ['a] and produces
   * values of type ['b].
  *)
  type 'a t
    constraint 'a = 'b -> 'c

  (** This is the type of elements that fittings know how to write to external
      processes. *)
  type 'a elem

 (** This is the parameter to the type {!elem} for specifying
  * the type of elements that fittings know how to read from
  * external processes.  That is, fittings constructed from external
  * processes produce values of type [initial elem]. *)
  type initial

  type 'a shtream
  type 'a coshtream

  (** Alias for {!Channel.procref} *)
  type procref   = Channel.procref

  (** Alias for {!initial} {!elem} *)
  type text = initial elem

  (** {1 Values} *)

  (** Connect the output of one fitting to the input of another.
   * This is the most basic fitting combinator, and bears introduction
   * before the full variety of fitting components. *)
  val ( -| )     : ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t

  val pipe       : ('i -> 'm) t -> ('m -> 'o) t -> ('i -> 'o) t
  (** Alias for {!(-|)} *)

  (** {2 Basic Fitting Constructors}
   *
   *  {3 Producers}
   *
   * Producers are useful for starting off pipelines.
  *)

  val from_file   : string -> ('i -> text) t
  (** Produce the contents of a file.
   * [from_file file -| fitting] is like {v     % fitting < file v} *)
  val from_null   : ('i -> text) t
  (** Produce nothing.
   * [from_null -| fitting] is like {v     % fitting < /dev/null v} *)
  val from_gen    : Channel.dup_in_source -> ('i -> text) t
  (** Produce the contents of a {!Channel.dup_in_source}. *)
  val from_shtream: 'o shtream -> ('i -> 'o) t
  (** Produce the contents of a shtream. *)

  (** {3 Consumers}
   *
   * Consumers are useful for ending pipelines.
   * *)

  val to_file     : ?clobber:Channel.clobber_spec ->
    string -> ('i elem -> 'o) t
  (** Redirect standard output to a file.
   * See {!Channel.clobber_spec} for open modes.
   * [fitting -| to_file file] is like {v     % fitting > file v} *)
  val to_null     : ('i elem -> 'o) t
  (** Redirect standard output nowhere.
   * [fitting -| to_null] is like {v     % fitting > /dev/null v} *)
  val to_stderr   : ('i elem -> 'o) t
  (** Redirect standard output to standard error.
      [fitting -| to_stderr] is like {v     % fitting >&2 v} *)
  val to_gen      : Channel.dup_out_source -> ('i elem -> 'o) t
  (** Redirect standard output to {!Channel.dup_out_source}. *)
  val to_coshtream: 'i coshtream -> ('i -> 'o) t
  (** Redirect output to a coshtream.  (A coshtream is a consumer
   * in another process.) *)

  (** {3 Transformers} *)

  (** Run an external command as a fitting.  The fitting's input is
   * connected to the command's standard input and the fitting's output
   * to the command's standard output.  This runs the command in the
   * shell, in the style of {!Channel.open_command}. *)
  val command     : string -> ('i elem -> text) t

  (** Run an external program as a fitting.  The fitting's input is
   * connected to the program's standard input and the fitting's output
   * to the program's standard output.  This runs the program in the
   * shell, in the style of {!Channel.open_program}. *)
  val program     : ?path:bool -> string -> ?argv0:string -> string list ->
    ('i elem -> text) t

  (** Run a thunk as a fitting.  The thunk is run in a child
   * process whose standard input and output are connected to the
   * fitting's input and output. *)
  val thunk       : (unit -> unit) -> ('i elem -> text) t

  (** Map each element according to a function.
   * This lifts a function on elements into a fitting component. *)
  val sed         : ('i -> 'o) -> ('i -> 'o) t

  (** Filter the input according to a predicate. *)
  val grep        : ('i -> bool) -> ('i -> 'i) t

  (** Transform the input according to a function on shtreams. *)
  val trans       : ('i shtream -> 'o shtream) -> ('i -> 'o) t

  (** Like {!sed} with a lift from strings. *)
  val sed_string  : (string -> 'o)   -> ('i elem -> 'o) t

  (** Filter the input according to a string predicate. *)
  val grep_string : (string -> bool) -> ('i elem -> 'i elem) t

  (** {2 Fitting Combinators} *)

  val ( /</ )     : (text -> 'o) t -> dup_spec -> (text -> 'o) t
  (** Redirect some inputs to a fitting.  [fitting /</ dups]
   * performs the redirections specifed by [dups] for the extent
   * of [fitting].  For example,
   * [fitting /</ \[ 4 %<& 0; 0 %< "file" \]] is like
   * {v     % fitting 4<&0 <file v}
  *)

  val redirect_in  : dup_spec -> (text -> 'o) t -> (text -> 'o) t
  (** Alias for {!(/</)} *)

  val ( />/ )     : ('i -> 'o elem) t -> dup_spec -> ('i -> 'o elem) t
  (** Redirect some outputs from a fitting.  [fitting />/ dups]
   * performs the redirections specifed by [dups] for the extent
   * of [fitting].  For example,
   * [fitting />/ \[ 2 %>& 1 \]] is like
   * {v     % fitting 2>&1 v}
  *)
  val redirect_out : dup_spec -> ('i -> 'o elem) t -> ('i -> 'o elem) t
  (** Alias for {!(/>/)} *)

  val (^>>=)      : ('i -> 'o) t ->
    (Proc.status -> ('i -> 'o) t) ->
    ('i -> 'o) t
  (** Sequence two fittings, with control.
   * Runs its first argument, passes
   * it's exit code to the second argument, and runs the resulting
   * fitting.  The second argument can therefore choose what to do
   * based on the result of the first.
   *
   * The exit code of external processes is the actual exit code as
   * reported by {!Proc.wait}.  The exit code of a shtream is 0 unless
   * the shtream terminates by calling [Shtream.fail_with n], in which
   * case the code in [n].  Or, {!yield} can return an exit code
   * directly.
  *)

  val seq         : ('i -> 'o) t ->
    (Proc.status -> ('i -> 'o) t) ->
    ('i -> 'o) t
  (** Alias for {!(^>>=)} *)

  val (^>>)       : ('i -> 'o) t -> ('i -> 'o) t -> ('i -> 'o) t
  (** Sequence two fittings, ignoring the exit code of the first.
   * [ a ^>> b ] is exactly [ a ^>>= fun _ -> b ].
   * This is like [;] in the shell. *)
  val (&&^)       : ('i -> 'o) t -> ('i -> 'o) t -> ('i -> 'o) t
  (** Sequence two fittings, running the second if the first succeeds.
   * If the first fails, skips the second and propagates the exit
   * code from the first.
   * This is like [&&] in the shell. *)
  val (||^)       : ('i -> 'o) t -> ('i -> 'o) t -> ('i -> 'o) t
  (** Sequence two fittings, running the second if the first fails.
   * If the first succeeds, skips the second and returns an exit code
   * of 0.
   * This is like [||] in the shell. *)

  val (~>>)       : ('i -> 'o) t list -> ('i -> 'o) t
  (** Run a list of fittings in sequence with {!(^>>)}. *)
  val (~&&)       : ('i -> 'o) t list -> ('i -> 'o) t
  (** Run a list of fittings in sequence with {!(&&^)}.
   * Terminates the sequence when any component fails. *)
  val (~||)       : ('i -> 'o) t list -> ('i -> 'o) t
  (** Run a list of fittings in sequence with {!(||^)}.
   * Terminates the sequence when any component succeeds. *)
  val commands    : string list     -> (text -> text) t
  (** Run a list of commands, piping the output of each
   * into the next. *)

  val yield      : Proc.status -> ('i -> 'o) t
  (** Produces a fitting that returns the given exit code.
   * Has no effect on input and output, but can be used to pass a
   * particular code along in a sequence.
  *)

  val caml       : (unit -> ('i -> 'o) t) -> ('i -> 'o) t
  (** Delay an OCaml thunk until a fitting is run.  Given a thunk
   * that produces a fitting, {!caml} constructs a new fitting that,
   * when run, forces the thunk and runs the resulting fitting.
   * This allows for OCaml side-effects at arbitrary points during a
   * fitting.
  *)

  val (^&=) : (text -> 'b elem) t -> (Proc.t -> ('i -> 'o) t) -> ('i -> 'o) t
  (** Run a fitting in the background.  [ bg ^&= fg ] runs [bg]
   * in the background, passed its {!Proc.t} to [fg], and runs
   * the fitting returned by [fg] (in the foreground).
   *
   * Notice that the [bg] must have input type {!text}; it will
   * construct its own input shtream from the standard input. *)
  val par   : (text -> 'b elem) t -> (Proc.t -> ('i -> 'o) t) -> ('i -> 'o) t
  (** Alias for {!(^&=)} *)

  val (^&)  : (text -> 'b elem) t -> ('i -> 'o) t             -> ('i -> 'o) t
  (** Run a fitting in the background, ignore its {!Proc.t}.  This
   * backgrounds its first argument and then continues with its second
   * argument in the foreground. *)

  (** {2 Fitting Runners} *)


  val run_source : (text -> 'o) t      -> 'o shtream
  (** Run a fitting, returning its output as a shtream.  The fitting
   * will take its input from the standard input. *)
  val run_sink   : ('i -> 'o elem) t   -> 'i coshtream
  (** Run a fitting, returning a costhream connected to its input.
   * The fitting will send its output from the standard output. *)
  val run_list   : (text -> 'o) t      -> 'o list
  (** Run a fitting, returning its output as a list.  The fitting
   * will take its input from the standard input. *)

  val run_shtream : ('i -> 'o) t  -> 'i shtream -> 'o shtream
  (**
   * Transform a fitting into a shtream transformer.
  *)

  val run_in     : ?procref:procref -> (text -> 'o elem) t -> in_channel
  (** Run a fitting, returning its output as an [in_channel].  The fitting
   * will take its input from the standard input.  If [?procref] is
   * provided, the {!Proc.t} of the child process will be stashed.
  *)
  val run_out    : ?procref:procref -> (text -> 'o elem) t -> out_channel
  (** Run a fitting, returning its input as an [out_channel].  The fitting
   * will send its output from the standard output.  If [?procref] is
   * provided, the {!Proc.t} of the child process will be stashed.
  *)

  val run_backquote : ?procref:procref -> (text -> 'o elem) t -> string
  (** Run a fitting, returning its output collected as a string.
   * The exit code of the child process can be retrieved by providing
   * [?procref]. *)

  val run_bg     : (text -> 'o elem) t -> Proc.t
  (** Run a fitting in the background, returning its {!Proc.t}.
   * The fitting will take its input from the standard input and send
   * its output to the standard output. *)
  val run        : (text -> 'o elem) t -> Proc.status
  (** Run a fitting in the foreground, returning its exit status.
   * The fitting will take its input from the standard input and send
   * its output to the standard output. *)

  (** {2 Convenient Conversions}

   * These conversions use the {!elem} conversions provided to the
   * {!Fitting.Make} functor by {!AnyShtreamSig.ELEM}.  The conversion
   * {!AnyShtreamSig.ELEM.string_of} or {!AnyShtreamSig.ELEM.of_string}
   * is completely applied for each of these conversions, so no state
   * (should there be any) is retained in between calls.
  *)

  val string_of_elem  : 'a elem -> string
  (** Convert a shtream element to a string. *)
  val elem_of_string  : string -> text
  (** Convert a string to a shtream element. *)
  val int_of_elem     : 'a elem -> int
  (** Convert a shtream element to an integer. *)
  val elem_of_int     : int -> text
  (** Convert a integer to a shtream element. *)
  val char_of_elem    : 'a elem -> char
  (** Convert a shtream element to a character. *)
  val elem_of_char    : char -> text
  (** Convert a character to a shtream element. *)
  val float_of_elem   : 'a elem -> float
  (** Convert a shtream element to a float. *)
  val elem_of_float   : float -> text
  (** Convert a float to a shtream element. *)
  val bool_of_elem    : 'a elem -> bool
  (** Convert a shtream element to a boolean. *)
  val elem_of_bool    : bool -> text
  (** Convert a boolean to a shtream element. *)
end
