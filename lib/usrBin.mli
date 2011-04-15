(**
 * High-level user utilities.
 *)

(** {2 Type Aliases} *)

type 'a fitting = 'a Fitting.t
  (** Alias for {!Fitting.t} *)
type 'a shtream = 'a Shtream.t
  (** Alias for {!Shtream.t} *)
type 'a line    = 'a Line.t
  (** Alias for {!Line.t} *)
type absent     = Line.absent
  (** Alias for {!Line.absent} *)
type present    = Line.present
  (** Alias for {!Line.present} *)

(** {2 Fitting Commands} *)

val ls       : string ->
               ('a ->
                <Line| stat: <| mode: present >;
                       source: present > line) fitting
  (** List a directory, with file metadata.  Given the name of a
   * directory, produces a fitting which outputs filenames with
   * {!Line.Stat} present. *)

val from_directory : string ->
                     ('a -> <Line| source: present > line) fitting
  (** Get the filenames in a directory.  Doesn't provide metadata. *)

val ps       : unit ->
                ('a -> 
                  <Line| ps: present; 
                         seq: present; 
                         source: present > line) fitting
  (** Get a information about currently running processes.
   * Returns a fitting which outputs {b ps}(1) output with process metadata
   * in the {!Line.Ps} structure. *)

val cut      : ('a line -> string) -> ('a Line.t -> 'a Line.t) fitting
  (** Select a particular field for each line passing through the fitting.
   *  Given a function to show lines, sets {!Line.show} for each line in the
   *  input. *)

val head : int -> ('a -> 'a) fitting
  (** [UsrBin.head n] is a fitting that only produces the first [n] 
   *  elements of its input. It leaves the rest for subsequent readers. *)

val head_while : ('a -> bool) -> ('a -> 'a) fitting
  (** A fitting that passes through elements satisfying a
   * predicate until encountering one that doesn't.
   * Leaves the remaining elements behind. *)

val behead     : int -> ('a -> 'a) fitting
  (** [UsrBin.behead n] is a fitting that drops the first [n] lines of
   * its input. *)

val behead_while : ('a -> bool) -> ('a -> 'a) fitting
  (** A fitting that deletes lines satisfying a predicate until reaching
   * one that doesn't. *)

val echo : string -> ('a -> Line.empty Line.t) fitting
  (** A fitting to print a string. *)

val renumber : int -> 
  (<Line| .. as 'a>  line ->
     <Line| seq: present; .. as 'a> line) fitting
  (** Update the sequence numbers in the lines passing through the pipeline
   *  to reflect the current sequence.  The optional argument [?from] 
   *  (default [0]) specifies the number for the first element. *)

val sort : ?compare:('a line -> 'a line -> int) -> unit ->
           ('a line -> 'a line) fitting
  (** Sort the lines coming into the fitting.  Since {!sort} must eagerly
   * consume its in order to sort it, attempts to sort infinite shtreams
   * will require patience. *)

val uniq : ?equal:('a line -> 'a line -> bool) -> unit ->
           ('a line -> 'a line) fitting
  (** Remove (adjacent) duplicate lines from the fitting's input.  When
   * several lines are equal according the the predicate [?equal]
   * (default compares {!Line.show}), discards all but the first.
   *)

(** {2 File Commands} *)

val isatty   : Channel.descr -> bool
  (** Is the given file descriptor a tty? *)

val set_stat : ?dir:string ->
               <Line| stat: absent;
                            .. as 'a > line ->
               <Line| stat: <| mode: present >;
                               source: present;
                               .. as 'a > line
  (** Add file metadata to a {!line}.  Uses the filename
   *  in {!Line.raw} *)

val stat     : string -> 
               <Line| stat: <| mode: present >; source: present > line
  (** Get file metadata for one file.
   * Creates a {!line} with {!Line.Stat} present. *)

(** Functions similar to the UNIX {b test}(1) command.  *)
module Test : sig
  val z : string -> bool
    (** [Test.z str] is true if str has length 0, false otherwise.  *)
  val n : string -> bool
    (** [Test.n str] is false if str has length 0, true otherwise.  *)

  val ef : string -> string -> bool
    (** [Test.ef f1 f2] is true if files [f1] and [f2] are the same. *)
  val nt : string -> string -> bool
    (** [Test.nt f1 f2] is true if file [f1] is newer than [f2]. *)
  val ot : string -> string -> bool
    (** [Test.ot f1 f2] is true if file [f1] is older than [f2]. *)

  val b : string -> bool
    (** [Test.b file] is true if [file] exists and is block special. *)
  val c : string -> bool
    (** [Test.c file] is true if [file] exists and is character special. *)
  val d : string -> bool
    (** [Test.d file] is true if [file] exists and is a directory. *)
  val f : string -> bool
    (** [Test.f file] is true if [file] exists and is a regular file. *)
  val h : string -> bool
    (** [Test.h file] is true if [file] exists and is a symbolic link. *)
  val p : string -> bool
    (** [Test.p file] is true if [file] exists and is a named pipe. *)
    
  val e : string -> bool
    (** [Test.e file] is true if [file] exists. *)
  val g : string -> bool
    (** [Test.g file] is true if [file] exists and is set-group-ID. *)
  val k : string -> bool
    (** [Test.k file] is true if [file] exists and has its sticky bit
        set. *)
  val s : string -> bool
    (** [Test.s file] is true if [file] exists and is non-empty. *)
  val u : string -> bool
    (** [Test.u file] is true if [file] exists and is set-user-ID. *)
  val t : int -> bool
    (** [Test.u n] is true if [n] is a tty file descriptor. *)
  val tfile : string -> bool
    (** [Test.u file] is true if [file] is readable and is a tty. *)

  val r : string -> bool
    (** [Test.r file] is true if [file] exists and read permission is
        granted. *)
  val w : string -> bool
    (** [Test.w file] is true if [file] exists and write permission is
        granted. *)
  val x : string -> bool
    (** [Test.x file] is true if [file] exists and execute permission is
        granted. *)
    
  val test :
    ([<
      | `And of 'a * 'a
      | `Char
      | `Dir
      | `Equal of string
      | `Execute
      | `Exists
      | `Link
      | `Newer of string
      | `NonEmpty
      | `Older of string
      | `Or of 'a * 'a
      | `Pipe
      | `Read
      | `Reg
      | `Sgid
      | `Sticky
      | `Suid
      | `Tty
      | `Write
      ] as 'a) -> string -> bool
    (** Evaluate a more elaborate {!Test} expression.
        For instance, one might write
        [Test.test `And (`Read,`NonEmpty) file] to test whether
        [file] is both readable and non-empty. *)
end

(** {2 Other Commands} *)

val backquote : string -> string
  (** Run a command and return its output. *)

val cd : string -> unit
  (** Change the current working directory. *)

val mkdir : string -> unit
  (** Create a directory. *)

val mkpath : string -> unit
  (** Create a directory path, succeeding even if some components
   *  exist.  This is like {b mkdir}(1) with the {i -p} option. *)

val pwd : unit -> string
  (** Find out the current working directory. *)

val sleep : int -> unit
  (** Sleep for the given number of seconds. *)
