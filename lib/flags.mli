(** Quick and dirty argument processing.  For full featured argument
 * processing, use [Arg], but if all you need is something very simple,
 * {!Flags} might do. *)

(** Raised by methods {!lookup.string} and {!lookup.int}
 * when the requested argument isn't present. *)
exception Argument_missing of string

(** The argument parser {!Flags.go} returns an object for querying
 * its results. *)
class type lookup = object
  method bool    : string -> bool
  (** Look up the value of a [bool] flag. *)
  method int     : ?default:int -> string -> int
  (** Look up the value of an [int] flag; if multiple values were given,
   * returns the last one. If the flag wasn't given but [?default] is
   * provided, returns that; otherwise throws {!Flags.Argument_missing}. *)
  method string  : ?default:string -> string -> string
  (** Look up the value of a [string] flag; if multiple values were given,
   * returns the last one. If the flag wasn't given but [?default] is
   * provided, returns that; otherwise throws {!Flags.Argument_missing}. *)

  method bcount  : string -> int
  (** Look up how many times a particular [bool] flag was given. *)
  method ints    : string -> int list
  (** Return all the values given with an [int] flag. *)
  method strings : string -> string list
  (** Return all the values given with a [string] flag.  Passing the
   * empty string [""] will return all additional (non-flag) arguments. *)

  method usage   : unit
  (** Print usage information on [stdout] *)
end

(** [Flags.go spec] parses the command-line arguments according to
 * [spec] and returns a [lookup] object.  If [?argv] is given, it uses
 * that rather than [Sys.argv]; if [?usage] is given, it uses that as
 * the sample command in the usage message.
 * 
 * The [spec] argument is a stylized sequence of arguments:
 * - ["-a"] and ["--apple"] specify boolean flags.
 * - ["-a <N>"] and ["--apple <Color>"] specify string arguments.
 * - ["-a <N:int>"] and ["--apple <Color:int>"] specify integer arguments.
 * 
 * Single-hyphen arguments consist of one alphabetic character, while
 * long (double-hyphen) arguments start with an alphabetic character
 * followed by an arbitrary number of alphanumerics.
 * 
 * For example, we might use
 * ["-q -v -b <Before:int> -a <After:int> --baz <Baz>"] to specify
 * a program that takes boolean flags ["-q"] and ["-v"], integer
 * arguments ["-b"] and ["-a"], and a string argument ["--baz"].  If we
 * pass this string to {!Flags.go} and it returns an object
 * [lookup], we may then query lookup, for example:
 * - [lookup#bool "-q"] checks whether ["-q"] was given.
 * - [lookup#int "-b"] returns an integer if ["-b"] was given, or raises
 * {!Flags.Argument_missing} otherwise.
 * - [lookup#ints "-a"] returns a list of integers (possibly 0 length) for
 * each time ["-a"] was given.
 * - [lookup#string ~default:"-" "--baz"] returns the provided string if
 * ["--baz"] was given, and ["-"] otherwise.
 *)
val go : ?argv:string array ->
         ?usage:string ->
         string -> lookup
