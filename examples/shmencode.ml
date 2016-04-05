#!/usr/bin/env ocamlscript
Ocaml.packs := [ "shcaml" ]
--

open Shcaml
open Fitting
open Adaptor
open UsrBin
open Reader
open Printf
open Channel.Dup

(* Individual track data. *)
type track = {
  index: int;
  title: string;
  wav:   string;
  mp3:   string;
}

let make_track index title = {
  index = index;
  title = title;
  wav   = sprintf "%02d %s.wav" index title;
  mp3   = sprintf "%02d %s.mp3" index title;
}

module CddbID : sig
  val discid : (int * int) list -> string
end = struct
  open Int32
  open List

  let ((+), (%), (/),      (<<<), (|||)) =
      (add, rem, div, shift_left, logor)

  let ten = of_int 10
  let fps = of_int 75

  let sum_digits =
    let rec loop acc n = if n = zero then acc else loop (acc + n % ten) (n / ten) in
      loop zero

  let discid track_list =
    let lengths = map (fun (x,_) -> of_int x) track_list in
    let offsets = map (fun (_,y) -> of_int y) track_list in
    let ntracks = of_int (length lengths) in
    let n  = fold_left (fun x y -> x + sum_digits (y / fps + of_int 2)) zero offsets in
    let t  = fold_left (+) zero lengths / fps in
    let id = (n % of_int 0xff <<< 24) ||| (t <<< 8) ||| ntracks in
      sprintf "%08lx" id
end

(* Getting the disc ID *)
module CdParanoia = Delim.Make_names(struct
  let options = { Delimited.default_options with
                    Delimited.field_sep = ' ' }
  let names   = [ "track"; "length"; "length-msh";
                  "begin"; "begin-msh"; "copy";
                  "pre"; "ch" ]
end)

let get_track_data () = run_list begin
  command "cdparanoia -Q 2>&1"
  -| grep_string (starts_with " ")
  -| CdParanoia.fitting ()
  -| sed (fun line -> (Line.Delim.get_int "length" line,
                       Line.Delim.get_int "begin" line))
end

let get_discid () = CddbID.discid (get_track_data ())


(* Getting the track info *)
let cddb_request discid =
  "http://freedb.freedb.org/~cddb/cddb.cgi" ^
  "?cmd=cddb+read+rock+" ^ discid ^ "&hello=" ^
  backquote "whoami" ^ "+" ^ backquote "hostname" ^
  "+shmendcode+0.1b&proto=6"

let curl url = program "curl" ["-s"; url]

let parse_album value =
  match Pcre.split ~pat:" / " ~max:2 value with
  | [ artist; album ] -> ["--ta"; artist; "--tl"; album]
  | _                 -> ["--ta"; value;  "--tl"; value]

let parse_track key value tracks =
  try Scanf.sscanf key "TTITLE%d"
      (fun n -> make_track (n + 1) value :: tracks)
  with _ -> tracks

let parse_cddb_line (tracks, album) line =
  let module KV = Line.Key_value in
  match KV.key line with
  | "DYEAR"  -> tracks, ["--ty"; KV.value line] @ album
  | "DGENRE" -> tracks, ["--tg"; KV.value line] @ album
  | "DTITLE" -> tracks, parse_album (KV.value line)
                                                @ album
  | key -> parse_track key (KV.value line) tracks, album

let get_cddb discid =
  let (tracks, album_tags) =
    Shtream.fold_left parse_cddb_line ([], [])
      (run_source begin
         curl (cddb_request discid)
         -| Key_value.fitting ~quiet:true ()
       end) in
  (List.rev tracks, album_tags)

(* Ripping and encoding. *)
let rip track =
  program "cdparanoia"
    ["--"; string_of_int track.index; track.wav]
    />/ [ 2 %>* `Null; 1 %>& 2 ]

let encode album_tags track =
  program "lame"
    (album_tags @
     ["--tn"; string_of_int track.index;
      "--tt"; track.title; "--quiet";
      track.wav; track.mp3])
  &&^ program "rm" [track.wav]

let build_dag (tracks, album) =
  let each (mp3s, prev) track =
    let wav = DepDAG.make ~prio:1 (fun _ ->
                printf "Ripping  %s\n%!" track.wav;
                run_bg (rip track)
              ) prev in
    let mp3 = DepDAG.make ~prio:2 (fun _ ->
                printf "Encoding %s\n%!" track.mp3;
                run_bg (encode album track)
              ) [wav] in
    (mp3::mp3s, [wav]) in
  let mp3s, _ = List.fold_left each ([], []) tracks in
    DepDAG.make_par mp3s

let main () =
  let opts     = Flags.go "-N <max-procs:int>" in
  let n        = opts#int ~default:2 "-N" in
  let discinfo = get_cddb (get_discid ()) in
    DepDAG.run ~n (build_dag discinfo)

;;
main ()
