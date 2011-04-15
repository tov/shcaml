(*
 * vim: sw=2 tw=70
 *)

(*
 * Linespec AST
 *)
open Camlp4.PreCast
open Syntax
open Ast

let nc_printer =
  let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
  new PP.printer ~comments:false ()

let printer =
  let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
  new PP.printer ()

let _ = printer#set_semisep "\n\n"
let _ = printer#set_loc_and_comments

let stdfmt     = Format.std_formatter
let errfmt     = Format.err_formatter
let newline () = Format.fprintf stdfmt "\n\n%!"

type variance = Invariant | Covariant | Contravariant
type type_decl =
  loc * string * (Loc.t * string * variance) list * ctyp * (ctyp * ctyp) list

type t = {
  loc      : Loc.t;
  doc      : string option;
  decls    : decl list;
}
and decl =
 | Default of default
 | Field   of field
and default = {
  dloc     : Loc.t;
  dtyp     : ctyp;
  dexp     : expr;
}
and field = 
 | Simple  of simple
 | Complex of complex
 | LetVal  of letval
 | Type    of typedec
and complex = {
  cloc     : Loc.t;
  cdoc     : string option;
  cname    : string;
  mname    : string;
  fields   : field list;
}
and simple = {
  sloc     : Loc.t;
  sdoc     : string option;
  name     : string;
  flag     : flag;
  typ      : ctyp;
  exp      : expr option;
}
and letval = {
  lloc     : Loc.t;
  ldoc     : string option;
  lname    : string;
  ltyp     : ctyp option;
  lexp     : expr;
}
and typedec = {
  tloc     : Loc.t;
  tdoc     : string option list;
  tdec     : type_decl list;
  tpvt     : bool;
  thid     : bool;
}
and flag = Option | Label | Explicit | Private | Nullable


(*
 * Linespec parser
 *
 * We use the functorial interface for parser creation, but we want
 * to inherit our lexer (and its keywords) from the Ocaml grammar.
 *)
(* module Gram = MakeGram(Lexer) *)

(*
 * In addition to our own main production (l_linespec), we need to
 * parse Ocaml expressions and types.
 *)

let l_linespec     = Gram.Entry.mk "l_linespec"
let l_linespec_eoi = Gram.Entry.mk "l_linespec_eoi"

EXTEND Gram
  GLOBAL: l_linespec l_linespec_eoi;

  l_linespec_eoi:
    [[ ls = l_linespec; `EOI -> ls ]]
    ;

  l_linespec:
    [[ doc = l_doc;
       entries = LIST0 [ d = l_default -> Default d
                       | f = l_field   -> Field f ]
         -> { loc = _loc; doc = doc; decls = entries; }
    ]]
    ;

  l_default:
    (* We use 'LIDENT "default"' rather than just '"default"' to avoid
     * polluting the lexer with new keywords. *)
    [[ "let"; LIDENT "default"; ":"; t = ctyp; "="; e = expr
         -> { dloc = _loc; dexp = e; dtyp = t; }
    ]]
    ;

  l_field:
    [[ "val"; (f, x) = l_field_helper; t = ctyp;
       e = OPT [ "="; e = expr -> e ]; d = l_doc
         -> Simple {
              sloc     = _loc;
              sdoc     = d;
              name     = x;
              flag     = f;
              typ      = t;
              exp      = e;
            }
     | "module"; x = UIDENT; "="; "struct";
        d = l_doc; f = LIST0 l_field; "end"
         -> Complex {
              cloc   = _loc;
              cdoc   = d;
              cname  = String.uncapitalize x;
              mname  = x;
              fields = f;
            }
     | "let"; x = LIDENT; t = OPT [ ":"; t = ctyp -> t ];
       "="; e = expr; d = l_doc
         -> LetVal {
              lloc   = _loc;
              ldoc   = d;
              lname  = x;
              ltyp   = t;
              lexp   = e;
            }
     | "type";
       p = [ "private" -> true | -> false ];
       h = [ "." -> true | -> false ];
       d = LIST1 [ a = type_declaration;
                   b = l_doc -> (a, b) ] SEP "and"
         -> let unQuo = function
                | TyQuo (loc, s) -> (loc, s, Invariant)
                | TyQuP (loc, s) -> (loc, s, Covariant)
                | TyQuM (loc, s) -> (loc, s, Contravariant)
                | _ -> failwith "linespec.ml: l_field bug (1)" in
            let unDcl = function
                | TyDcl (x, y, z, w, v), _ -> (x, y, List.map unQuo z, w, v)
                | _ -> failwith "linespec.ml: l_field bug (2)" in
            Type {
              tloc   = _loc;
              tdoc   = List.map snd d;
              tdec   = List.map unDcl d;
              tpvt   = p;
              thid   = h;
         }
    ]]
    ;

  (* This is gross!  But apparently ~ and ? are whitespace sensitive
   * in the lexer, so we need to handle any token it might produce. *)
  l_field_helper:
    [[ "?"; x = LIDENT; ":"        -> (Option, x)
     | x = OPTLABEL                -> (Option, x)
     | "~"; x = LIDENT; ":"        -> (Label, x)
     | x = LABEL                   -> (Label, x)
     | f = [ [LIDENT "option"   | "?" ] -> Option
           | [LIDENT "label"    | "~" ] -> Label
           | [       "private"  | "." ] -> Private
           | [LIDENT "explicit" | "!" ] -> Explicit
           | [LIDENT "nullable" |     ] -> Nullable ];
       x = LIDENT; ":"             -> (f, x)
    ]]
    ;

    l_doc:
    [[ "with"; d = STRING -> Some d
     | -> None
    ]]
    ;
END

(*
 * Now we're going to generate Caml AST.  Get ready . . .
 *)

let default def = function
  | None   -> Some def
  | s      -> s

let sprintf = Printf.sprintf
let printf  = Printf.printf

(* In this module, we define a function that walks the AST for a type,
 * erasing all position information -- this allows us to then compare
 * types for (crude) structural equality.  Yes, this is very ugly. *)
module CtypMap = Map.Make(struct
  let p = Loc.ghost
  let map = List.map

  let rec t = function
    | TyNil (_)       -> TyNil (p)
    | TyAli (_, x, y) -> TyAli (p, t x, t y)
    | TyAny _         -> TyAny p
    | TyApp (_, x, y) -> TyApp (p, t x, t y)
    | TyArr (_, x, y) -> TyArr (p, t x, t y)
    | TyCls (_, x)    -> TyCls (p, i x)
    | TyLab (_, x, y) -> TyLab (p, x, t y)
    | TyId  (_, x)    -> TyId  (p, i x)
    | TyMan (_, x, y) -> TyMan (p, t x, t y)
    | TyDcl (_, x, y, z, w)
                      -> let f (a, b) = (t a, t b) in
                         TyDcl (p, x, map t y, t z, map f w)
    | TyObj (_, x, y) -> TyObj (p, t x, y)
    | TyOlb (_, x, y) -> TyOlb (p, x, t y)
    | TyPol (_, x, y) -> TyPol (p, t x, t y)
    | TyQuo (_, x)    -> TyQuo (p, x)
    | TyQuP (_, x)    -> TyQuP (p, x)
    | TyQuM (_, x)    -> TyQuM (p, x)
    | TyVrn (_, x)    -> TyVrn (p, x)
    | TyRec (_, x)    -> TyRec (p, t x)
    | TyCol (_, x, y) -> TyCol (p, t x, t y)
    | TyCom (_, x, y) -> TyCom (p, t x, t y)
    | TySem (_, x, y) -> TySem (p, t x, t y)
    | TySum (_, x)    -> TySum (p, t x)
    | TyOf  (_, x, y) -> TyOf  (p, t x, t y)
    | TyAnd (_, x, y) -> TyAnd (p, t x, t y)
    | TyOr  (_, x, y) -> TyOr  (p, t x, t y)
    | TyPrv (_, x)    -> TyPrv (p, t x)
    | TyMut (_, x)    -> TyMut (p, t x)
    | TyTup (_, x)    -> TyTup (p, t x)
    | TySta (_, x, y) -> TySta (p, t x, t y)
    | TyVrnEq (_, x)  -> TyVrnEq (p, t x)
    | TyVrnSup (_, x) -> TyVrnSup (p, t x)
    | TyVrnInf (_, x) -> TyVrnInf (p, t x)
    | TyVrnInfSup (_, x, y)
                      -> TyVrnInfSup (p, t x, t y)
    | TyAmp (_, x, y) -> TyAmp (p, t x, t y)
    | TyOfAmp (_, x, y)
                      -> TyOfAmp (p, t x, t y)
    | TyAnt (_, x)    -> TyAnt (p, x)
  and i = function
    | IdAcc (_, x, y) -> IdAcc (p, i x, i y)
    | IdApp (_, x, y) -> IdApp (p, i x, i y)
    | IdLid (_, x)    -> IdLid (p, x)
    | IdUid (_, x)    -> IdUid (p, x)
    | IdAnt (_, x)    -> IdAnt (p, x)

  type t = ctyp
  let compare x y = compare (t x) (t y)
end)

(*
 * Default values for some Ocaml types.  These are used to initialize
 * line fields if no default is given.
 *)
let default_type_map =
  let _loc = Loc.ghost in
    List.fold_left
      (fun m (k, v) -> CtypMap.add k v m)
      CtypMap.empty
      [
        <:ctyp< bool >>,         <:expr< False >>;
        <:ctyp< char >>,         <:expr< '\000' >>;
        <:ctyp< int >>,          <:expr< 0 >>;
        <:ctyp< string >>,       <:expr< "" >>;
        <:ctyp< 'a option >>,    <:expr< None >>;
        <:ctyp< 'a list >>,      <:expr< [] >>;
        <:ctyp< in_channel >>,   <:expr< stdin >>;
        <:ctyp< out_channel >>,  <:expr< stdout >>;
      ]

(*
 * We need a special AST with room for docstrings.
 *)

type doc_sig = loc * string option * doc_item list
and doc_item =
  | DocOne  of loc * string option * sig_item
  | DocType of loc * string option list * type_decl list
  | DocMany of loc * doc_item list
  | DocStr  of loc * string * doc_sig

let doc_sig _loc doc items : doc_sig = _loc, doc, items
let doc_one _loc str item            = DocOne (_loc, (Some str), item)
let doc_one_opt _loc str item        = DocOne (_loc, str, item)
let doc_type _loc docs decls         = DocType (_loc, docs, decls)
let doc_many _loc items              = DocMany (_loc, items)
let doc_str _loc name cont           = DocStr (_loc, name, cont)

let tydcl_of_type_decl =
  let revar = function
      | loc, n, Invariant     -> TyQuo (loc, n)
      | loc, n, Covariant     -> TyQuP (loc, n)
      | loc, n, Contravariant -> TyQuM (loc, n) in
  function (x, y, z, w, v) -> TyDcl (x, y, List.map revar z, w, v)

let sig_item_of_doctype _loc _ decls =
  <:sig_item< type $list: List.map tydcl_of_type_decl decls$ >>

(* This function generates p4 AST from our docstring AST *)
let rec mli_of_doc_sig (_loc, _, items) =
  <:module_type<
    sig
      $list: List.map mli_of_doc_item items $
    end
  >>
and mli_of_doc_item = function
  | DocOne (_, _, item) ->
      item
  | DocType (_loc, docs, decls) ->
      sig_item_of_doctype _loc docs decls
  | DocMany (_loc, items) ->
      <:sig_item<
         $list: List.map mli_of_doc_item items $
      >>
  | DocStr (_loc, name, doc_sig) ->
      <:sig_item<
        module $name$ :
          $mli_of_doc_sig doc_sig$
      >>

let print_doc_string = function
  | None   -> ()
  | Some s -> printf "(** %s *)\n" s

let print_location loc =
  match Loc.to_tuple loc with
  | (_, n, _, _, _, _, _, false) ->
      Printf.printf "# %d \"lib/line.ls\"\n" n
  | _ -> ()

(* This function prints our doc ast, including docstrings in comments
 * for Ocamldoc. *)
let rec print_doc_sig
    ((_loc, doc, items) : doc_sig) : unit =
  print_location _loc;
  print_doc_string doc;
  print_newline ();
  List.iter print_doc_item items
and print_doc_item : doc_item -> unit = function
  | DocOne (_loc, doc, item) ->
      print_doc_string doc;
      print_location _loc;
      printer#sig_item stdfmt item;
      newline ()
  | DocType (_loc, docs, decls) ->
      print_location _loc;
      let first = ref true in
      List.iter2 (fun doc ((_loc, _, _, _, _) as decl) ->
          print_doc_string doc;
          print_location _loc;
          Format.fprintf stdfmt "%s " (if !first then "type" else "and");
          first := false;
          printer#ctyp stdfmt (tydcl_of_type_decl decl);
          Format.fprintf stdfmt "\n")
        docs decls;
      newline ();
      printf "\n\n"
  | DocMany (_loc, items) ->
      print_location _loc;
      List.iter print_doc_item items
  | DocStr (_loc, name, (_, doc, items)) ->
      print_doc_string doc;
      print_location _loc;
      printf "module %s : sig\n\n" name;
      List.iter print_doc_item items;
      printf "end\n\n"

(* This function prints a structure. *)
let print_str = function
  | MeStr (loc, items) ->
      printer#str_item stdfmt items;
      newline ()
  | _ -> failwith "Unexpected (2)"

(*
 * Helpers for getting info out of our AST
 *)

let maption f lst =
  let f' x xs = match f x with
                | None -> xs
                | Some x' -> x' :: xs
    in List.fold_right f' lst []

let fields_of_decls = 
  let just_field = function
    | Field f -> Some f
    | _       -> None in
  maption just_field

let flag_fields flag = maption (function
 | Simple f when f.flag = flag
     -> Some f
 | _ -> None)

let defaults_of_linespec ls =
  let _loc = ls.loc in
  let map =
    List.fold_left
    (fun m x -> match x with
       | Default d -> CtypMap.add d.dtyp d.dexp m
       | _         -> m)
    default_type_map ls.decls in
  fun t -> try CtypMap.find t map
    with Not_found ->
      Format.fprintf errfmt "Warning: Default expression for type `";
      nc_printer#ctyp errfmt t;
      Format.fprintf errfmt "' not found\n";
      Format.fprintf errfmt "Warning: Using Obj.magic instead\n%!";
      <:expr< Obj.magic 0 >>

(*
 * Now things get really ugly.  Best not to read the rest of the file.
 *)

let make_obj_type _loc = function
 | []      -> <:ctyp< present >>
 | methods ->
     let tycol (lab, ty) = <:ctyp< $lid: lab$ : $ty$ >> in
       <:ctyp< < $list: List.map tycol methods$ > >>

let displayed_fields_of_fields =
  maption (function
   | Simple {
       sloc     = _loc;
       name     = n;
       flag     = Nullable;
     } -> Some (_loc, n)
   | Complex {
       cloc     = _loc;
       cname    = n;
     } -> Some (_loc, n)
   | _ -> None)

let empty_record_of_fields _loc fields =
  let make_method (_loc, n) = (n, <:ctyp< absent >>) in
  let displayed = displayed_fields_of_fields fields in
    make_obj_type _loc (List.map make_method displayed)

let make_row _loc ?present ?value names =
  let ty n = if present = Some n
    then match value with
         | None   -> <:ctyp< present >>
         | Some t -> t
    else <:ctyp< '$n$ >> in
  let each n = (n, ty n) in
    make_obj_type _loc (List.map each names)

let make_create_type _loc fields result =
  let def_arg field acc = 
     <:ctyp< $TyOlb (_loc, field.name, field.typ)$ -> $acc$ >> in
  let lab_arg field acc =
     <:ctyp< $TyLab (_loc, field.name, field.typ)$ -> $acc$ >> in
  let exp_arg field acc =
     <:ctyp< $field.typ$ -> $acc$ >> in
    List.fold_right def_arg (flag_fields Option fields)
      (List.fold_right lab_arg (flag_fields Label fields)
        (List.fold_right exp_arg (flag_fields Explicit fields) result))

let rec acc_of_field root swrap mwrap fields =
  let names = maption
    (function
     | Simple { flag = Nullable; name = n; }
                              -> Some n
     | Complex { cname = n; } -> Some n
     | _                      -> None) fields in
  let any _loc = match root, names with
    | true, _  -> <:ctyp< _ >>
    | _   , [] -> <:ctyp< present >>
    | _        -> <:ctyp< < .. > >> in
  function
  | Simple {
      sloc     = _loc;
      flag     = Private;
    } -> doc_many _loc []
  | Simple {
      sloc     = _loc;
      sdoc     = d;
      flag     = Nullable;
      name     = n;
      typ      = t;
    } -> let sel_row = swrap (TyObj(_loc, <:ctyp< $lid:n$: present >>, BTrue))
         in
         doc_many _loc [
           doc_one _loc
           (match d with
            | Some s -> sprintf "%s (accessor, nullable)" s
            | _      -> "Accessor for nullable field")
           <:sig_item< value $n$ : $sel_row$ -> $t$ >>;

           doc_one _loc
           (sprintf "Updater for {!%s}" n)
           <:sig_item<
             value $"set_"^n$ :
               $t$ ->
               $mwrap (make_row _loc names)$ ->
               $mwrap (make_row _loc ~present:n names)$
           >>;
         ]
  | Simple {
      sloc     = _loc;
      sdoc     = d;
      name     = n;
      typ      = t;
      exp      = e;
    } ->
         doc_many _loc [
           doc_one _loc
           (let def = match e with
              | Some exp ->
                  let buf = Buffer.create 10 in
                  let fmt = Format.formatter_of_buffer buf in
                    Format.fprintf fmt ", default = [";
                    nc_printer#expr fmt exp;
                    Format.fprintf fmt "]%!";
                    Buffer.contents buf
              | _ -> "" in
            match d with
              | Some s ->
                  sprintf "%s (accessor, required%s)" s def
              | _ ->
                  sprintf "Accessor for required field%s" def)
           <:sig_item< value $n$ : $swrap (any _loc)$ -> $t$ >>;

           doc_one _loc
           (sprintf "Updater for {!%s}" n)
           <:sig_item<
             value $"set_"^n$ :
               $t$ ->
               ($swrap (any _loc)$ as 'line) ->
               'line
           >>
         ]
  | Complex {
      cloc   = _loc;
      cdoc   = d;
      cname  = n;
      mname  = module_name;
      fields = f;
    } -> let swrap x = swrap (TyObj(_loc, <:ctyp< $lid:n$: $x$ >>, BTrue)) in
         let mwrap x = mwrap (make_row _loc
                                ~present:n
                                ~value:x
                                names) in
         let arr a r = <:ctyp< $a$ -> $r$ >> in
         doc_str _loc module_name
         (_loc, default "A line substructure" d, [
           doc_many _loc (List.map (acc_of_field false swrap mwrap f) f);

           doc_one _loc
             (sprintf "Add the {!%s} substructure to a line." module_name)
           <:sig_item<
             value create :
               $make_create_type _loc f
                 (arr (mwrap <:ctyp< absent >>)
                      (mwrap (empty_record_of_fields _loc f)))$
                      >>;
         ])
  | LetVal {
      lloc  = _loc;
      ldoc  = d;
      lname = n;
      ltyp  = Some t;
    } -> doc_one_opt _loc d <:sig_item< value $lid:n$ : $t$ >>
  | LetVal { lloc = _loc; }
  -> doc_many _loc []
  | Type {
      thid  = false;
      tpvt  = false;
      tloc  = _loc;
      tdoc  = docs;
      tdec  = d;
    } -> doc_type _loc docs d
  | Type {
      thid  = false;
      tpvt  = true;
      tloc  = _loc;
      tdoc  = docs;
      tdec  = d;
    } -> let middle (_, s, _) = s in
         let rhs ps = "abs_" ^ String.concat "" (List.map middle ps) in
         let erase (_loc, name, params, _, _) =
                   (_loc, name, params, TyQuo (_loc, rhs params), []) in
         doc_type _loc docs (List.map erase d)
  | Type {
      thid  = true;
      tloc  = _loc;
    } -> doc_many _loc []

let doc_sig_of_linespec ls =
  let _loc   = ls.loc in
  let fields = fields_of_decls ls.decls in
  let wrap x = <:ctyp< t $x$ >> in
    _loc, default "Structured lines" ls.doc, [
      doc_one _loc
      "Phantom type used to indicate that a {!Line.t} field is present."
      <:sig_item< type present = private [< `Phantom ] >>;

      doc_one _loc
      "Phantom type used to indicate that a {!Line.t} field is absent.
       This is a supertype of {!Line.present} so that the presence of
       fields may be cast away."
      <:sig_item< type absent = private [> `Phantom ] >>;

      doc_one _loc
      "The type of a structured line.
       The parameter indicates which fields are present."
      <:sig_item< type t +'a >>;

      doc_many _loc
      (List.map (acc_of_field true wrap wrap fields) fields);

      doc_one _loc
      "The {!Line.t} parameter for a line with no metadata."
      <:sig_item< type empty = $empty_record_of_fields _loc fields$ >>;

      doc_one _loc
      "Construct an empty line from a string."
      <:sig_item< value line: $make_create_type _loc fields
                                 (wrap <:ctyp< empty >>)$ >>;
    ]

let mli_of_linespec ls = mli_of_doc_sig (doc_sig_of_linespec ls)

let tysubst_of_linespec ls =
  let module TySubst = Map.Make(struct
        type t = string list
        let compare = compare end) in
  let gensym s i = "_" ^ s ^ "_" ^ string_of_int i ^ "_" in
  let rec ts ty fields (count, map) =
    List.fold_left
      (fun (count, map) -> function
         | Simple {
             name   = n;
           } -> (count + 1, TySubst.add (n :: ty) (gensym n count) map)
         | Complex {
             cname  = n;
             fields = f;
           } -> let map = TySubst.add (n :: ty) (gensym n count) map in
                  ts (n :: ty) f (count + 1, map)
         | LetVal _ | Type _
             -> (count, map))
      (count, map)
      fields in
  let (_, map) = ts [] (fields_of_decls ls.decls) (1, TySubst.empty) in
  fun ty -> try TySubst.find ty map
    with Not_found -> failwith "Unexpected: Renaming for field not found."

let type_decls_of_linespec ls =
  let _loc  = ls.loc in
  let subst = tysubst_of_linespec ls in
  let make_rec curr = <:ctyp< { $list: curr$ } >> in
  let make_decl ty curr = match ty with
    | [] -> TyDcl(_loc, "t", [TyQuP (_loc, "a")], make_rec curr, [])
    | _  -> TyDcl(_loc, subst ty, [], make_rec curr, []) in
  let rec tdol ty fields acc =
    match
      List.fold_left
        (fun (curr, rest) each ->
           match each with
           | Simple {
               sloc  = _loc;
               name  = n;
               typ   = t;
             } -> (<:ctyp< $lid: subst (n::ty)$ : $t$ >> :: curr, rest)
           | Complex {
               cloc  = _loc;
               cname = n;
               fields = f;
             } -> let ty' = n :: ty in
                    (<:ctyp< $lid: subst ty'$ : $lid: subst ty'$ >> :: curr,
                     tdol ty' f rest)
            | Type {
               tdec  = d;
             } -> (curr, List.map tydcl_of_type_decl d @ rest )
           | LetVal _
               -> (curr, rest))
        ([], acc)
        fields
      with (curr, rest) -> make_decl ty curr :: rest in
    let fields = fields_of_decls ls.decls in
    tdol [] fields
         [TyDcl(_loc, "empty", [], empty_record_of_fields _loc fields, [])]

let mod_decls_of_linespec ls =
  let defaults   = defaults_of_linespec ls in
  let default_expr_of_field f
                 = match f.exp with
                   | None   -> defaults f.typ
                   | Some e -> e in
  let subst      = tysubst_of_linespec ls in
  let field _loc ty    = IdLid (_loc, subst ty) in
  let modempty _loc m = <:expr< $uid: m$ . empty >> in
  let rec sel_path _loc path = match path with
    | []      -> <:expr< r >>
    | _::rest -> <:expr< $sel_path _loc rest$ . $lid: subst path $ >> in
  let rec mut_path _loc path acc = match path with
    | []      -> acc
    | _::rest -> mut_path _loc rest
                  <:expr< { ( $sel_path _loc rest$ ) with
                            $lid: subst path$ = $acc$ } >> in
  let field_accessors _loc n path =
        <:str_item<
             value $lid:n$ r = $sel_path _loc (n::path)$;
             value $lid:"set_"^n$ n r =
               $mut_path _loc (n::path) <:expr< n >>$;
        >> in
  let let_value _loc n e =
    <:str_item< value $lid:n$ = $e$ >> in
  let make_empty_object _loc path fields =
    <:expr<
      {
        $list:
          List.map
            (function
             | Simple f
                 -> RbEq (_loc, field f.sloc (f.name :: path),
                                default_expr_of_field f)
             | Complex c
                 -> RbEq (_loc, field c.cloc (c.cname :: path),
                                modempty c.cloc c.mname)
             | LetVal _ | Type _
                 -> <:rec_binding< >>
            )
          fields
        $
      } 
    >> in
  let make_create_function _loc path fields wrap =
    let rename field = subst (field.name :: path) in
    let record =
      <:expr<
        {
          $list:
            List.map
              (function
               | Simple f
                   -> RbEq (_loc,
                            field f.sloc (f.name :: path),
                            if f.flag <> Private && f.flag <> Nullable then
                              <:expr< $lid: rename f$ >>
                            else
                              default_expr_of_field f)
               | Complex c
                   -> RbEq (_loc,
                            field c.cloc (c.cname :: path),
                            modempty c.cloc c.mname)
               | LetVal _ | Type _
                   -> <:rec_binding< >>
              )
            fields
          $
        } 
      >> in
    let def_arg field acc =
      let d = default_expr_of_field field in
      let pattern = <:patt< $lid: rename field$ >> in
         <:expr< fun $PaOlbi (_loc, field.name, pattern, d)$ -> $acc$ >> in
    let lab_arg field acc =
      let pattern = <:patt< $lid: rename field$>> in
      <:expr< fun $PaLab (_loc, field.name, pattern)$ -> $acc$ >> in
    let exp_arg field acc =
      <:expr< fun $lid: rename field$ -> $acc$ >> in
      List.fold_right def_arg (flag_fields Option fields)
        (List.fold_right lab_arg (flag_fields Label fields)
          (List.fold_right exp_arg (flag_fields Explicit fields)
            (wrap record))) in
  let make_type_aliases _loc =
    let alias (_loc, name, _, _, _) =
      let dec1 = TyDcl(_loc, "root"^name, [], <:ctyp< $lid:name$ >>, []) in
      let dec2 = TyDcl(_loc, name, [], <:ctyp< $lid:"root"^name$ >>, []) in
          <:str_item<
            type $dec1$;
            type $dec2$;
          >> in
    fun type_decls ->
      <:str_item< $list:
        List.map alias type_decls
      $ >> in
  let rec make_submodule _loc n path fields =
    let path = n :: path in
    let wrap record = <:expr< fun r -> $mut_path _loc path record$ >> in
    <:str_item<
      module $String.capitalize n$ = struct
        $mdol _loc path fields $;
        value create  = $make_create_function _loc path fields wrap$;
      end
    >>
  and mdol _loc path fields =
    <:str_item<
        $list:
          List.map
            (function
             | Complex {
                 cloc   = _loc;
                 cname  = n;
                 fields = f;
               } -> make_submodule _loc n path f
             | Simple {
                 sloc = _loc;
                 name = n;
               } -> field_accessors _loc n path
             | LetVal {
                 lloc  = _loc;
                 lname = n;
                 lexp  = e;
               } -> let_value _loc n e
             | Type {
                 tdec  = d;
                 tloc  = _loc;
               } when path <> []
                 -> make_type_aliases _loc d
             | Type {
                 tloc  = _loc;
               } -> <:str_item< >>
            )
          fields
        $;
        value empty = $ make_empty_object _loc path fields $;
    >> in
    let fields = fields_of_decls ls.decls in
    let _loc   = ls.loc in
      <:str_item<
          $mdol _loc [] fields$;
          value line = $make_create_function _loc [] fields (fun x -> x)$;
      >>

let ml_of_linespec ls =
  let _loc   = ls.loc in
    <:module_expr< struct
      type present = [= `Phantom ];
      type absent  = [= `Phantom ];

      type $list: type_decls_of_linespec ls $;

      $mod_decls_of_linespec ls$;
    end >>

let data_of_linespec ls =
  let _loc = ls.loc in
    <:module_expr< struct
      value linespec : Linespec.t =
        Marshal.from_string
          $str: String.escaped (Marshal.to_string ls [])$ 0;
    end >>
