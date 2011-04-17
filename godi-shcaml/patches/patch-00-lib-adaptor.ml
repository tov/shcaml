--- lib/adaptor.ml.orig
+++ lib/adaptor.ml
@@ -211,7 +211,7 @@
 
   let splitter line =
     match split_helper ~max:7 ':' (Line.raw line) with
-    | [| name; passwd; uid; gid; gecos; home; shell; |] ->
+    | [| name; passwd; uid; gid; gecos; home; shell |] ->
         Line.Passwd.create ~name ~passwd ~gecos ~home ~shell
                            ~uid:(Convert.to_int loc uid)
                            ~gid:(Convert.to_int loc gid) line
@@ -264,9 +264,9 @@
 
   let splitter line =
     match split_string (Line.raw line), "0", "0" with
-    | [| file_system; mount_point; fstype; options; dump; pass; |], _, _
-    | [| file_system; mount_point; fstype; options; dump; |], _, pass
-    | [| file_system; mount_point; fstype; options; |], dump, pass ->
+    | [| file_system; mount_point; fstype; options; dump; pass |], _, _
+    | [| file_system; mount_point; fstype; options; dump |], _, pass
+    | [| file_system; mount_point; fstype; options |], dump, pass ->
         Line.Fstab.create ~file_system ~mount_point ~fstype
                           ~options:(split_options options)
                           ~dump:(Convert.to_int loc dump)
@@ -345,7 +345,7 @@
     if Line.seq line = 0 then Shtream.try_again ();
     match split_string (Line.raw line) with
     | [| user; pid; pcpu; pmem; vsz; rss;
-         tt; stat; started; time; command; |] ->
+         tt; stat; started; time; command |] ->
         Line.Ps.create 
           ~user
           ~pcpu:(Convert.to_float loc pcpu)
