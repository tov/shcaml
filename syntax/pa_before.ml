open Camlp4.PreCast
open Syntax

EXTEND Gram
  expr: LEVEL "&&"
  [[ e1 = expr; "BEFORE"; e2 = expr
      -> <:expr< let it = $e1$ in do { Pervasives.ignore $e2$; it } >>
  ]]
  ;
END
