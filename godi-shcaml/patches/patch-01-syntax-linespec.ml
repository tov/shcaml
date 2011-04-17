--- syntax/linespec.ml.orig
+++ syntax/linespec.ml
@@ -251,7 +251,6 @@
     | TyOfAmp (_, x, y)
                       -> TyOfAmp (p, t x, t y)
     | TyAnt (_, x)    -> TyAnt (p, x)
-    | TyPkg (_, x)    -> TyPkg (p, x)
   and i = function
     | IdAcc (_, x, y) -> IdAcc (p, i x, i y)
     | IdApp (_, x, y) -> IdApp (p, i x, i y)
@@ -495,7 +494,7 @@
       name     = n;
       typ      = t;
     } -> let sel_row = swrap (TyObj(_loc, <:ctyp< $lid:n$: present >>,
-                                    RvRowVar))
+                                    BTrue))
          in
          doc_many _loc [
            doc_one _loc
@@ -554,7 +553,7 @@
       mname  = module_name;
       fields = f;
     } -> let swrap x = swrap (TyObj(_loc, <:ctyp< $lid:n$: $x$ >>,
-                                    RvRowVar)) in
+                                    BTrue)) in
          let mwrap x = mwrap (make_row _loc
                                 ~present:n
                                 ~value:x
