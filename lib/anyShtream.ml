(* vim: set ft=ocaml : *)
open Util

module type ELEM = sig
  type 'a elem
  type initial
  val reader    : unit -> in_channel -> initial elem
  val of_string : unit -> string -> initial elem
  val string_of : unit -> 'a elem -> string
end

module type S = sig
  include Shtream.COMMON
    with type 'a t = 'a Shtream.t
     and type 'a co_t = 'a Shtream.co_t

  module Elem : ELEM

  type 'a elem = 'a Elem.elem
  type initial = Elem.initial

  val elem_reader : Reader.t -> (in_channel -> initial elem)
  val output      : ?channel:out_channel ->
    ?init:('a elem -> string) ->
    ?term:('a elem -> string) ->
    ?show:('a elem -> string) ->
    'a elem t ->
    unit
  val channel_of  : ?procref:Channel.procref ->
    ?before:(unit -> unit) ->
    ?after:(unit -> unit) ->
    ?init:('a elem -> string) ->
    ?term:('a elem -> string) ->
    ?show:('a elem -> string) ->
    'a elem t -> in_channel
  val string_list_of      : ?show:('a elem -> string) ->
    'a elem t -> string list
  val string_stream_of    : ?show:('a elem -> string) ->
    'a elem t -> string Stream.t
  val of_channel  : ?reader:(in_channel -> initial elem) ->
    in_channel -> initial elem t
  val of_file     : ?reader:(in_channel -> initial elem) ->
    string -> initial elem t
  val of_command  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    string ->
    initial elem t
  val of_program  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    ?path:bool -> string -> ?argv0:string -> string list ->
    initial elem t
  val of_thunk  : ?procref:Channel.procref ->
    ?dups:Channel.dup_spec ->
    ?reader:(in_channel -> initial elem) ->
    (unit -> unit) ->
    initial elem t
  val of_string_list   : ?parse:(string -> initial elem) ->
    string list -> initial elem t
  val of_string_stream : ?parse:(string -> initial elem) ->
    string Stream.t -> initial elem t
end

module Make (E : ELEM) : (S with module Elem = E) = struct
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
