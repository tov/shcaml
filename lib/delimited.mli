(** Parsers for delmited text formats, especially CSV.  This module
 * provides a record reader, field splitter, and printer.
 * The delimiter, quoting, and whitespace behavior are all
 * configurable. *)

(** Options for parsing delimited text files. *)
type options = {
  field_sep: char;
  (** The field separator character.  (Default [',']) *)
  record_sep: char;
  (** The record separator character.  (Default ['\n']) *)
  trim_space: bool;
  (** Whether to remove whitespace from the beginning and end of each
   * field. (Default [true]) *)
  rec_backslash: bool;
  (** A backslash quotes the next character. (Default [false]) *)
  rec_quotation: bool;
  (** Recognize double quotes. (Default [true]) *)
  rec_double_double: bool;
  (** Within quotation marks, two double quotes in a row
   * denote one double quote.  (Default [true]) *)
  rec_cr: bool;
  (** Treat a carriage return that precedes a record
   * separator as part of the separator.  This, along with setting
   * the record separator to ['\n'], will treat MS-DOS CRLF as
   * a record separator. *)
  rec_escapes: bool;
  (** Recognize backslash sequences such as ["\\n"] for newline
   * and ["\\t"] for tab. *)
  max_fields: int;
  (** The maximum number of fields to split into.  Field separators
   * and fields subsequent to this are all concatenated in the last
   * field.  A value of [0] (the default) means unlimited. *)
}

(** The default options.  To read records with the default options but
 * also recognizing backslash, one might write
 * [Csv.reader ~options:{default_options with recognize_backslash = true}].
 *)
val default_options : options

(** The CSV reader.  Splits a file into records, but doesn't interpret
 * any quoting; that is, you get out what you put in. *)
val reader   : ?options:options -> in_channel -> Reader.raw_line

(** The CSV splitter.  Given a single CSV record, splits it into
 * fields and interprets quoting and escaping properly to recover
 * the original strings. *)
val splitter : ?options:options -> string -> string array

(** Output a single field, escaped as necessary for CSV. *)
val output_field : ?options:options -> out_channel -> string -> unit

(** Output a CSV record, including the newline. *)
val output_record : ?options:options -> out_channel -> string array -> unit

