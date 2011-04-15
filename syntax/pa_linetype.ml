open MLast
open Pcaml
open Linespec

type linetype = {
  loc:    MLast.loc;
  fields: (string * ftyp) list;
  rowvar: string option
}
and ftyp = Rec of linetype | Type of ctyp

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
  | None   -> make_type _loc "absent" path
  | Some a -> let id = a^"_"^ String.concat "_" path ^"_"^name in
              <:ctyp< '$id$ >>

let rec build_type path lt =
  let knowns = List.map (function
    | name, Rec lt' -> name, build_type (path@[String.capitalize name]) lt'
    | name, Type t  -> name, t) lt.fields in
  let has_field n = List.exists (fun n', _ -> n = n') knowns in
  let check acc (_loc, name) =
    if has_field name then acc
    else
      let ty = make_var_or_absent _loc path name lt.rowvar in
        (name, ty) :: acc in
  match List.fold_left check knowns (fields_of_path linespec path) with
  | []    -> make_type lt.loc "present" path
  | pairs -> TyObj (lt.loc, pairs, false)

EXTEND
  GLOBAL: ctyp ;

  ctyp: LEVEL "simple"
    [[ "<"; path = LIST0 UIDENT SEP "."; "|"; lt = shcaml_linetype; ">"
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
         -> { loc = _loc; fields = []; rowvar = Some r }
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
