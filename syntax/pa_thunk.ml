open Camlp4.PreCast
open Syntax

EXTEND Gram
  expr: LEVEL "simple"
  [[ "{|"; e = expr; "|}"
     -> <:expr< fun _ -> $e$ >>
  ]]
  ;
END
