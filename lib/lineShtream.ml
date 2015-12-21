open Util

type sourced = <Line| seq: Line.present; source: Line.present >

let annotate source =
  let counter = ref 0 in
  let each line =
    let seq = !counter in
      counter := seq + 1;
      Line.set_seq seq @@ Line.set_source source line in
  Shtream.map each

let parse_raw_lines source =
  let counter = ref 0 in
  fun raw ->
    let seq = !counter in
      counter := seq + 1;
      Line.set_seq seq @@
        Line.set_source source @@
          Line.line ~before:raw.Reader.before
                    ~after:raw.Reader.after
                    raw.Reader.content

let line_reader ?(source = `None) reader =
  parse_raw_lines source % reader

module LineElem = struct
  type 'a elem  = 'a Line.t
  type initial  = sourced
  let reader    () = line_reader Reader.lines
  let string_of () = Line.show
  let of_string () = line_reader Reader.raw_of_string
end

include AnyShtream.Make(LineElem)

let elem_reader reader = line_reader reader

let output ?channel
           ?(init = Line.before)
           ?(term = Line.after)
           ?(show = Line.show) =
  output ?channel ~init ~term ~show

let channel_of ?procref ?before ?after
               ?(init = Line.before)
               ?(term = Line.after)
               ?(show = Line.show) =
  channel_of ?procref ?before ?after ~init ~term ~show

(* Helper for defaulting the reader and making the resulting shtream
 * hintable. *)
let default_reader reader source
                   (kont : ?hint:(Reader.raw_line -> 'b) -> 'a) =
  match reader with
  | None   -> let hint = parse_raw_lines source in
              kont ~hint (line_reader ~source Reader.lines)
  | Some r -> kont r

let of_channel ?reader channel =
  default_reader reader (`Other "channel")
                 Shtream.of_channel channel

let of_file ?reader file =
  default_reader reader (`File file)
                 Shtream.of_file file

let of_command ?procref ?dups ?reader cmd =
  default_reader reader (`Command cmd)
                 (Shtream.of_command ?procref ?dups) cmd

let of_program ?procref ?dups ?reader ?path prog ?argv0 args =
  default_reader reader (`Process (Proc.execspec ?path prog ?argv0 args))
                 (Shtream.of_program ?procref ?dups)
                 ?path prog ?argv0 args

let source_parse ?parse source =
  maybe parse (fun () -> line_reader ~source Reader.raw_of_string) id

let of_string_list ?parse lst =
  let parse = source_parse ?parse (`Other "list") in
    of_string_list ~parse lst

let of_string_stream ?parse lst =
  let parse = source_parse ?parse (`Other "stream") in
    of_string_stream ~parse lst
