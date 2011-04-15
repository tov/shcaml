#! /usr/bin/env ocamlscript
Ocaml.packs := [ "shcaml" ]
--

(* Tests for lib/csv.ml *)

open Shcaml
open Util
open Csv
open Printf

let raised_unexp = ref 0
let didn't_raise = ref 0
let didn't_match = ref 0
let total_tests  = ref 0
let failed_tests = ref 0

let content x = x.Reader.content

let prerr_options options =
  eprintf "{ ";
  if options.field_sep <> default_options.field_sep then
    eprintf "field_sep = '%c'; " options.field_sep;
  if options.record_sep <> default_options.record_sep then
    eprintf "record_sep = '%c'; " options.record_sep;
  if options.trim_space <> default_options.trim_space then
    eprintf "trim_space = %b; " options.trim_space;
  if options.rec_backslash <> default_options.rec_backslash then
    eprintf "rec_backslash = %b; " options.rec_backslash;
  if options.rec_quotation <> default_options.rec_quotation then
    eprintf "rec_quotation = %b; " options.rec_quotation;
  if options.rec_double_double <>
     default_options.rec_double_double then
    eprintf "rec_double_double = %b; "
      options.rec_double_double;
  if options.rec_cr <>
     default_options.rec_cr then
    eprintf "rec_cr = %b; "
      options.rec_cr;
  if options.rec_escapes <>
     default_options.rec_escapes then
    eprintf "rec_escapes = %b; "
      options.rec_escapes;
  eprintf " }\n"

let prerr_records recs =
  eprintf "[\n";
  List.iter (fun record ->
    eprintf "  [| ";
    Array.iter (eprintf "\"%s\"; " % String.escaped) record;
    eprintf "|]\n") recs;
  eprintf "]\n"

let roundtrip good options recs =
  total_tests := !total_tests + 1;
  match try Some (Channel.with_out_string (fun outc ->
                    List.iter (output_record ~options outc) recs))
        with Failure _ -> None with
    | None when good options ->
        raised_unexp := !raised_unexp + 1;
        failed_tests := !failed_tests + 1;
        printf "Houston, we threw unexpectedly:\n  ";
        prerr_options options;
        prerr_newline ()
    | None -> eprintf "*"
    | Some (_, serialized) when not (good options) ->
        didn't_raise := !didn't_raise + 1;
        failed_tests := !failed_tests + 1;
        printf "Houston, we succeeded unexpectedly:\n  ";
        prerr_options options;
        eprintf "<<<%s>>>\n" serialized;
        prerr_newline ()
    | Some (_, serialized) ->
        let inc = Channel.open_string_in serialized in
        let get = splitter ~options % content % reader ~options in
        let rec loop acc =
          match try Some (get inc) with
          | End_of_file -> None
          | e           ->
              eprintf "\nOops, bailing out\n";
              prerr_options options;
              prerr_records recs;
              eprintf "<<<%s>>>\n" serialized;
              raise e
          with
          | Some record -> loop (record :: acc)
          | None        -> List.rev acc in
        let back = loop [] in
        if back = recs then 
          eprintf "."
        else begin
          didn't_match := !didn't_match + 1;
          failed_tests := !failed_tests + 1;
          eprintf "Houston, we have a problem:\n  ";
          prerr_options options;
          prerr_records recs;
          eprintf "<<<%s>>>\n" serialized;
          prerr_records back;
          prerr_newline ()
        end;
        close_in inc

let concat_map = Shtream.concat_map

let option_set =
  Shtream.list_of ^$
  concat_map (fun o -> [ {o with field_sep = ','};
                         {o with field_sep = ' '};
                         {o with field_sep = 'v'};
                         {o with field_sep = '\t'}; ]) ^$
  concat_map (fun o -> [ {o with record_sep = '\n'};
                         {o with record_sep = '\000'}; ]) ^$
  concat_map (fun o -> [ {o with trim_space = true};
                         {o with trim_space = false}; ]) ^$
  concat_map (fun o -> [ {o with rec_backslash = false};
                         {o with rec_backslash = true}; ]) ^$
  concat_map (fun o -> [ {o with rec_quotation = true};
                         {o with rec_quotation = false}; ]) ^$
  concat_map (fun o -> [ {o with rec_double_double = true};
                         {o with rec_double_double = false}; ]) ^$
  concat_map (fun o -> [ {o with rec_cr = true};
                         {o with rec_cr = false}; ]) ^$
  concat_map (fun o -> [ {o with rec_escapes = false};
                         {o with rec_escapes = true}; ]) ^$
  Shtream.of_list [ default_options ]
;;

let good options = if String.contains " \t\r\n" options.field_sep &&
                      options.trim_space then
                     options.rec_quotation
                   else true in
flip List.iter option_set ^$ fun options ->
  roundtrip good options [
    [| "a"; ""; "b" |];
    [| "c"; "" |];
    [| ""; "c" |];
    [| ""; "" |];
  ]
;;

let good options = true in
flip List.iter option_set ^$ fun options ->
  roundtrip good options [
    [| "a" |];
    [| "a"; "bc"; "de" |];
  ]
;;

let good options = if options.field_sep = ' ' then
                       options.rec_backslash or
                       options.rec_escapes or
                       options.rec_quotation
                     else true in
flip List.iter option_set ^$ fun options ->
  roundtrip good options [
    [| "a" |];
    [| "a"; "b\\c"; "d e" |];
  ]
;;

let good options = if options.field_sep = ' ' ||
                      options.trim_space then
                       options.rec_backslash or
                       options.rec_escapes or
                       options.rec_quotation
                     else true in
flip List.iter option_set ^$ fun options ->
  roundtrip good options [
    [| "a" |];
    [| "a"; "b\\c"; "d e" |];
    [| " a"; "bc "; " " |];
    [| "  a"; "bc  "; "  "; "   " |];
  ]
;;

let good options = options.rec_backslash or
                   options.rec_escapes or
                   options.rec_quotation in
flip List.iter option_set ^$ fun options ->
  roundtrip good options [
    [| "a"; "b\\c"; "d e" |];
    [| " a\n"; "b,c "; " " |];
    [| "  va"; "bc  "; "  "; " \n \t  " |];
    [| " \006\\" |]
  ]
;;

let good options = options.rec_backslash or
                   options.rec_escapes or
                   (options.rec_quotation &&
                    (options.rec_double_double or
                     options.rec_backslash or
                     options.rec_escapes)) in
flip List.iter option_set ^$ fun options ->
  roundtrip good options [
    [| "a"; "b\\c"; "d\000 e" |];
    [| " a\n"; "b,c "; " " |];
    [| "  va"; "b\"c  "; "  "; " \n \t  " |];
    [| " \006"; "\\\""; "\"\"\\\"" |];
  ]
;;

prerr_newline ();  
prerr_newline ();  
eprintf "raised unexpectedly:        %d\n" !raised_unexp;
eprintf "didn't raise when expected: %d\n" !didn't_raise;
eprintf "didn't match when expected: %d\n" !didn't_match;
prerr_newline ();
eprintf "failed tests:               %d\n" !failed_tests;
eprintf "total tests:                %d\n" !total_tests;
()
