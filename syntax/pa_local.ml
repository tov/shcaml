open Pcaml
open MLast

let gensym =
  let count = ref 0 in
    fun () ->
      count := !count + 1;
      Printf.sprintf "LocalModule%06x" !count

module StrSet = Set.Make(struct
  type t = string
  let compare = String.compare
end)

let rec names_of_patt _loc set = function
| <:patt< ($p1$ as $p2$) >>  -> names_of_patt _loc
                                  (names_of_patt _loc set p1) p2
| <:patt< $f$ $_$ >>         -> names_of_patt _loc set f
| <:patt< $lid: n$ >>        -> StrSet.add n set
| <:patt< ($p$ : $_$) >>     -> names_of_patt _loc set p
| <:patt< ($list: patts$) >> -> List.fold_left (names_of_patt _loc) set patts
| _                          -> set

let names_of_str_item _loc set = function
| StVal (_, _, bindings) -> List.fold_left
                              (names_of_patt _loc)
                              set
                              (List.map fst bindings)
| _                      -> set

let names_of_str_items _loc =
  List.fold_left (names_of_str_item _loc) StrSet.empty

let gen_local _loc locals exports =
  let modname = gensym () in
  let names   = StrSet.elements (names_of_str_items _loc exports) in
  let assigns = List.map
    (fun n -> <:str_item< value $lid: n$ = $uid: modname$ . $lid: n$ >>)
    names in
    <:str_item<
      declare
        module $uid: modname$ = struct
          declare
            $list: locals$
          end;
          declare
            $list: exports$
          end;
        end;

        declare
          $list: assigns$
        end;
      end
    >>

EXTEND 
  str_item:
    [[ "LOCAL"; locs = LIST1 str_item; "IN";
       exps = LIST1 str_item; "END"
          -> gen_local _loc locs exps
    ]]
    ;
END
