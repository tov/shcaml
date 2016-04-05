(* vim: set ft=ocaml : *)
open Util

module Make (E : AnyShtreamSig.ELEM) = struct
  include Shtream

  module Elem = E

  type 'a elem = 'a Elem.elem
  type initial = Elem.initial

  let elem_reader reader =
    let of_string = Elem.of_string () in
      fun c -> of_string (reader c).Reader.content

  let output ?(channel = stdout)
             ?(init = fun _ -> "")
             ?(term = fun _ -> "\n")
             ?(show = Elem.string_of ()) =
    iter (fun elt ->
            output_string channel (init elt);
            output_string channel (show elt);
            output_string channel (term elt);
            flush channel)

  let channel_of ?procref ?before ?after
                 ?(init = fun _ -> "")
                 ?(term = fun _ -> "\n")
                 ?(show = Elem.string_of ()) =
    channel_of ?procref ?before ?after
      (fun elt ->
         print_string (init elt);
         print_string (show elt);
         print_string (term elt))

  let string_list_of ?(show = Elem.string_of ()) =
    list_of   % map show

  let string_stream_of ?(show = Elem.string_of ()) =
    stream_of % map show

  (* This little helper function either passes through the given reader,
   * or selects a default and indicates that the resulting shtream
   * should be susceptible to hints. *)
  let default_reader reader (kont : ?hint:(Reader.raw_line -> 'a) -> 'b) =
    match reader with
    | None   -> let parse  = Elem.of_string () in
                let hint r = parse r.Reader.content in
                kont ~hint (Elem.reader ())
    | Some r -> kont r

  let of_channel ?reader =
    default_reader reader of_channel

  let of_file ?reader =
    default_reader reader of_file

  let of_command ?procref ?dups ?reader =
    default_reader reader (of_command ?procref ?dups)

  let of_program ?procref ?dups ?reader =
    default_reader reader (of_program ?procref ?dups)

  let of_thunk ?procref ?dups ?reader =
    default_reader reader (of_thunk ?procref ?dups)

  let of_string_list   ?(parse = Elem.of_string ()) = map parse % of_list
  let of_string_stream ?(parse = Elem.of_string ()) = map parse % of_stream
end
