(* We can make linespec a syntax extension (rather than using our own
 * preprocessor). *)

open MLast
open Pcaml
open Linespec

let c_linespec = Grammar.Entry.of_parser gram "c_linespec"
                   (Gram.Entry.parse_token l_linespec) in
EXTEND
  module_expr:
    [[ "LINESPEC"; ls = c_linespec; "END" ->
         <:module_expr<
           ($ml_of_linespec ls$ : $mli_of_linespec ls$)
         >>
    |
      "LINESPEC"; "NOSIG"; ls = c_linespec; "END" ->
         <:module_expr<
           $ml_of_linespec ls$
         >>
    ]]
    ;
  module_type:
    [[ "LINESPEC"; ls = c_linespec; "END" ->
         <:module_type<
           $mli_of_linespec ls$
         >>
    ]]
    ;
END
