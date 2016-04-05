(** Readers are responsible for breaking input data into records.
 * A reader need not be concerned with the meaning of data {i in} a
 * record.  Its goal is merely to determine record boundaring.  Given an
 * [in_channel] from which to read, a reader must produce a string
 * containing one record from the channel, and should also keep track of
 * any non-data (formatting or record separators) that it encounters,
 * making its operation somewhat invertible.
 *)

(** An untyped record as returned by a reader *)
type raw_line = {
  content : string; (** The data of the record *)
  before  : string; (** Delimiting text from before the data *)
  after   : string; (** Delimiting text from after the data *)
}

(** The type of a reader.  A reader extracts {!raw_line}s from an input
 * channel. *)
type t = in_channel -> raw_line

(** Construct an untyped record from its contents.  The optional
 * parameter [?before] defaults to [""] and [?after] defaults to
 * ["\n"]. *)
val raw_of_string : ?before:string -> ?after:string -> string -> raw_line

(** Construct a reader with a predefined behavior.
 * - [`Char c] means that records are terminated by the character [c].
 * - [`Set s] means that records are separated by a sequence of one or
 * more characters from the string [s].  Record separator characters may
 * be returned in either the preceding or following {!raw_line}.
 * - [`Fixed (n, m)] means that records comprise [n] characters of data
 * followed by [m] characters of garbage.
 * - [`Buf f] uses the function [f] to determine whether the input
 * buffer contains a complete record.  If [f] returns [Some r], then [r]
 * is returns the buffer is flushed; otherwise, one more character is
 * read and then [f] is tried again.  At end-of-file, [f] is passed
 * [~eof:true].
 *
 * Readers constructed by {!make} are stateless between calls.
 * *)
val make     : [ `Char  of char
               | `Set   of string
               | `Fixed of int * int
               | `Buf   of eof:bool -> Buffer.t -> raw_line option ]
               -> t

(** Read newline-terminated lines.  If the last line is not
 * newline-terminated, a newline is stored in the trailing delimiter
 * nonetheless. *)
val lines        : t

(** {2 Reader Transformers}

    Reader transformers add behavior to a reader. *)

val ignore_if    : (string -> bool) -> t -> t
(** Ignore records satisfying a string predicate.  Given a predicate and
 * a reader, returns a new reader that skips records whose content
 * satisfies the predicate. *)
val join_on      : char -> t -> t
(** Read records with a line continuation character.  If a record
 * ends with the given character, the character will be removed and the
 * next record will be concatenated. *)

val empty        : string -> bool
(** Predicate for empty strings. *)
val blank        : string -> bool
(** Predicate for empty or white space strings. *)
val starts_with  : string -> string -> bool
(** Predicate for strings starting with a given string.
 * [starts_with patt s] returns whether [s] starts with the string [patt].
 * Allows additional leading white space in the subject string.
 *)
val ends_with    : string -> string -> bool
(** Predicate for strings ending with a given string.
 * [ends_with patt s] returns whether [s] ends with the string [patt].
 * Allows additional trailing white space in the subject string.
 *)
val contains     : ?regexp:bool -> string -> string -> bool
(** Predicate for strings containing a given pattern.
 * [contains patt s] returns whether the string [s] contains the
 * {i string} [patt].  Calling [contains ~regexp:true patt s]
 * returns whether the string [s] matches the Perl-compatible regular
 * expression [patt].
 *)
