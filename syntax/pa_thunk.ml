open Pcaml
open MLast

EXTEND
  expr: LEVEL "simple"
  [[ "{|"; e = expr; "|}"
     -> <:expr< fun _ -> $e$ >>
  ]]
  ;
END
