(* We can make linespec a syntax extension (rather than using our own
 * preprocessor). *)

open Linespec
open Camlp4.PreCast
open Syntax
open Ast

EXTEND Gram
  module_expr:
    [[ "LINESPEC"; ls = l_linespec; "END" ->
         <:module_expr<
           ($ml_of_linespec ls$ : $mli_of_linespec ls$)
         >>
    |
      "LINESPEC"; "NOSIG"; ls = l_linespec; "END" ->
         <:module_expr<
           $ml_of_linespec ls$
         >>
    ]]
    ;
  module_type:
    [[ "LINESPEC"; ls = l_linespec; "END" ->
         <:module_type<
           $mli_of_linespec ls$
         >>
    ]]
    ;
END
