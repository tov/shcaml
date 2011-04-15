open Pcaml
open MLast

EXTEND
  expr: LEVEL "&&"
  [[ e1 = expr; "BEFORE"; e2 = expr
      -> <:expr< let it = $e1$ in do { Pervasives.ignore $e2$; it } >>
  ]]
  ;
END
