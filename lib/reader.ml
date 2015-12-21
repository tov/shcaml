open Util

type raw_line = {
  content  : string;
  before   : string;
  after    : string;
}

let raw_of_string ?(before="") ?(after = "\n") s = {
  content  = s;
  before   = before;
  after    = after;
}

type t = in_channel -> raw_line

let lines = raw_of_string % input_line
  (* This adds \n to the end of the last line even if the
     input ends without a newline *)

let rec buf_span buf pred start limit =
  if start < limit && pred (Buffer.nth buf start)
  then buf_span buf pred (start + 1) limit
  else start

let rec buf_rspan buf pred start limit =
  if limit > start && pred (Buffer.nth buf (limit - 1))
  then buf_rspan buf pred start (limit - 1)
  else limit

let set_helper set ~eof buf =
  let pred = String.contains set in
  let limit  = Buffer.length buf in
  if eof then
    let pre   = buf_span buf pred 0 limit in
    let post  = buf_rspan buf pred pre limit in
    Some { content  = Buffer.sub buf pre (post - pre);
           before   = Buffer.sub buf 0 pre;
           after    = Buffer.sub buf post (limit - post); }
  else if limit > 1 &&
          pred (Buffer.nth buf (limit - 1)) &&
          not (pred (Buffer.nth buf (limit - 2))) then
    let pre = buf_span buf pred 0 limit in
    Some { content  = Buffer.sub buf pre (limit - pre - 1);
           before   = Buffer.sub buf 0 pre;
           after    = Buffer.sub buf (limit - 1) 1; }
  else None

let rec make =
  function
  | `Char chr -> fun c ->
      let buf = Buffer.create 80 in
      let rec loop () =
          match input_char c with
          | x when (x <> chr) -> Buffer.add_char buf x; loop ()
          | x -> { content  = Buffer.contents buf;
                   before   = "";
                   after    = String.make 1 x } in
      (try loop ()
         with End_of_file when Buffer.length buf <> 0
           -> { content  = Buffer.contents buf;
                before   = "";
                after    = ""; })
  | `Set set -> make (`Buf (set_helper set))
  | `Fixed (n, m) -> fun c ->
      let result = { content  = String.make n '\000';
                     before   = "";
                     after    = String.make m '\000'; } in
      Pervasives.really_input c result.content 0 n;
      ignore @@ Pervasives.input c result.after 0 n;
      result
  | `Buf f -> fun c ->
      let buf = Buffer.create 80 in
      let rec loop () =
        Buffer.add_char buf (input_char c);
        match f ~eof:false buf with
        | None    -> loop ()
        | Some rl -> rl in
      try loop ()
      with End_of_file as e when Buffer.length buf <> 0
        -> match f ~eof:true buf with
           | None    -> raise e
           | Some rl -> rl

let rec ignore_if pred reader c =
  let rl = reader c in
    if pred rl.content
    then ignore_if pred reader c
    else rl

    (* This could be optimized, but it's not necessary unless there are
     * lines that get continued many times. *)
let join_on character reader c =
  let rec loop this =
    let length = String.length this.content in
    if this.content.[length - 1] = character
    then let next = reader c in
      loop { before  = this.before;
             content = String.sub this.content 0 (length - 1) ^ next.content;
             after   = next.after; }
    else this in
  loop (reader c)

let empty               = (=) ""

let contains ?(regexp=false) pat =
  let rex = Pcre.regexp (if regexp then pat else Pcre.quote pat) in
    fun str -> Pcre.pmatch ~rex str

let blank = contains ~regexp:true "^\\s*$"

let starts_with pat = contains ~regexp:true ("^\\s*" ^ Pcre.quote pat)
let ends_with pat   = contains ~regexp:true (Pcre.quote pat ^ "\\s*$")
