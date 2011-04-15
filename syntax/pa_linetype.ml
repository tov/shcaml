open Camlp4.PreCast
open Syntax
open Ast

open Linespec

type linetype = {
  loc:    loc;
  fields: (string * ftyp) list;
  rowvar: rowvar option
}
and ftyp = Rec of linetype | Type of ctyp
and rowvar = Anon | Named of string

let linespec = LineMetadata.linespec

let make_type _loc tycon = function
  | "Shcaml"::"Line"::_ ->
        <:ctyp< Shcaml . Line . $lid: tycon$ >>
  | "Line"::_ ->
        <:ctyp< Line . $lid: tycon$ >>
  | _ ->
        <:ctyp< $lid: tycon$ >>

let fields_of_path ls path =
  let rec find_fields x = function
    | Complex c :: _ when c.mname = x -> c.Linespec.fields
    | _ :: rest -> find_fields x rest
    | [] -> failwith ("pa_linetype.cmo: could not locate line module "
                      ^ String.concat "." path) in
  let rec helper curr = function
    | []    -> displayed_fields_of_fields curr
    | x::xs -> helper (find_fields x curr) xs in
  match path with
  | "Shcaml"::"Line"::path | "Line"::path | path
    -> helper (fields_of_decls ls.decls) path

let make_var_or_absent _loc path name = function
  | None ->
      make_type _loc "absent" path
  | Some (Named a) ->
      let id = a^"_"^ String.concat "_" path ^"_"^name in
      <:ctyp< '$id$ >>
  | Some Anon ->
      <:ctyp< _ >>

let rec build_type path lt =
  let col (_loc, name, ty) = <:ctyp< $lid: name$ : $ty$ >> in
  let knowns = List.map (function
    | name, Rec lt' -> lt'.loc, name,
                       build_type (path@[String.capitalize name]) lt'
    | name, Type t  -> loc_of_ctyp t, name, t) lt.fields in
  let has_field n = List.exists (fun (_, n', _) -> n = n') knowns in
  let check acc (_loc, name) =
    if has_field name then acc
    else
      let ty = make_var_or_absent _loc path name lt.rowvar in
        (_loc, name, ty) :: acc in
  let _loc = lt.loc in
  match lt with
  | { fields = []; rowvar = Some Anon } -> <:ctyp< < .. > >>
  | _ ->
  match List.fold_left check knowns (fields_of_path linespec path) with
  | []    -> make_type lt.loc "present" path
  | pairs -> <:ctyp< < $list: List.map col pairs$ > >>

EXTEND Gram
  GLOBAL: ctyp ;

  ctyp: LEVEL "simple"
    [[ "<"; "|"; lt = shcaml_linetype; ">"
         -> build_type ["Shcaml"; "Line"] lt
     | "<"; path = LIST1 [ uid = UIDENT -> uid ] SEP ".";
       "|"; lt = shcaml_linetype; ">"
         -> build_type path lt
     | "<|"; lt = shcaml_linetype; ">"
         -> build_type ["Shcaml"; "Line"] lt
    ]]
    ;

  shcaml_linetype:
    [[ name = LIDENT; ":"; ftyp = shcaml_ftyp; rest = shcaml_fields
         -> { rest with fields = (name, ftyp)::rest.fields;
                        loc    = _loc }
     | row  = shcaml_row
         -> row
     |   -> { loc = _loc; fields = []; rowvar = None; }
    ]]
    ;

  shcaml_fields:
    [[ ";"; name = LIDENT; ":"; ftyp = shcaml_ftyp; rest = shcaml_fields
         -> { rest with fields = (name, ftyp) :: rest.fields }
     | OPT ";"; row = shcaml_row
         -> row
     | OPT ";"
         -> { loc = _loc; fields = []; rowvar = None }
    ]] 
    ;

  shcaml_row:
    [[ ".."; "as"; "'"; r = LIDENT
         -> { loc = _loc; fields = []; rowvar = Some (Named r) }
     | ".."
         -> { loc = _loc; fields = []; rowvar = Some Anon }
    ]]
    ;

  shcaml_ftyp:
    [[ ["<|" | "<"; "|"]; lt = shcaml_linetype; ">"
         -> Rec lt
     | typ = ctyp
         -> Type typ
    ]]
    ;
END
