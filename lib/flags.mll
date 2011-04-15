{
  exception Argument_missing of string

  let arg_missing s = raise (Argument_missing s)

  type opt = 
    | Bool   of int ref
    | String of string list ref
    | Int    of int list ref

  class lookup find args usage = object
    method bool k = match find k with
      | Bool r -> !r > 0
      | _ -> arg_missing k

    method int ?default k = match find k with
      | Int { contents = v :: _ } -> v
      | _ -> match default with
             | Some v -> v
             | _      -> arg_missing k

    method string ?default k = match find k with
      | String { contents = v :: _ } -> v
      | _ -> match default with
             | Some v -> v
             | _      -> arg_missing k

    method bcount k = match find k with
      | Bool r -> !r
      | _ -> 0

    method ints k = match find k with
      | Int r -> List.rev !r
      | _ -> []

    method strings k = match find k with
      | String r -> List.rev !r
      | _ -> []

    method usage = Arg.usage args usage;
                   flush stdout
  end

  module SM = Map.Make(String)

  let argmap   = ref SM.empty
  let add k v  = argmap := SM.add k v !argmap
  let clear () =
    let anon  = ref [] in
      argmap := SM.empty;
      add "" (String anon);
      fun s -> anon := s :: !anon
                 
  let find () =
    let saved = !argmap in
      fun k -> SM.find k saved
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = alpha (alpha | digit | '-')*
let ws = (' ' | '\t')+

let flag = ('-' alpha | "--" id)

rule opts = parse
  | flag as opt
      {
        let r      = ref 0 in
        let set () = r := !r + 1 in
          add opt (Bool r);
          Some (opt, Arg.Unit set, " Boolean")
      }

  | (flag as opt) ('='|ws) '<' (id as name) '>'
      {
        let r     = ref [] in
        let set s = r := s :: !r in
          add opt (String r);
          Some (opt, Arg.String set, " "^name)
      }

  | (flag as opt) ('='|ws) '<' (id as name) ":int>"
      {
        let r     = ref [] in
        let set i = r := i :: !r in
          add opt (Int r);
          Some (opt, Arg.Int set, " "^name)
      }

  | ws           { opts lexbuf } (* eat whitespace *)
  | eof          { None }
  | _            { raise (Invalid_argument "Flags.go") }
    
{
  let start = !Arg.current

  let rec parse lexbuf = 
    match opts lexbuf with
    | Some o -> o :: parse lexbuf
    | None   -> []

  let to_opts str = parse (Lexing.from_string str)

  let go ?argv ?usage str =
    let anon  = clear () in
    let rest  = ["--", Arg.Rest anon, " End option processing"] in
    let args  = Arg.align(to_opts str @ rest) in
    let usage = Printf.sprintf "Usage: %s %s" Sys.argv.(0)
                  (match usage with Some s -> s | _ -> str) in
    let argv, current = match argv with
      | Some a -> a, ref 0
      | None   -> Sys.argv, ref start in
      try
        Arg.parse_argv ~current:(ref 0) argv args anon usage;
        new lookup (find ()) args usage
      with
      | Arg.Bad s  -> prerr_string s; exit 2
      | Arg.Help h -> print_string h; exit 0
}
