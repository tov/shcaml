#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;
#use "config.ml";;

let () =
  let e = [`Ext ".cmi"; `Ext ".cmti"; `Ext ".cmx"; `Ext ".cma"; `Ext ".cmxa"; `Ext ".cmxs"] in
  Vars.subst_file ~skip:Config.subst_skip ~vars:Config.vars "pkg/META" >>& fun () ->
  Vars.subst_file ~skip:Config.subst_skip ~vars:Config.vars "lib/version.ml" >>& fun () ->
  Pkg.describe "shcaml" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:e "shcaml";
    Pkg.lib ~exts:e "shcamlCommon";
    Pkg.lib ~exts:Exts.module_library "shtop";
    Pkg.lib ~exts:e "shtopInit";
    Pkg.lib "dir_shcaml.ml"; ]
