#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let codename = "Shmaltz"
(* shlock shadken shanda shaygets shayner shiksa
   shlemiel shlep shmendrick shmegegge
   shmutzik shnorrer shtik shtetl shtunk
   shvitz

   Used:
   0.1.0 shmatta
   0.1.1 shmooz
   0.1.2 shlimazl
   0.1.3 shmeer
   0.2.0 shmaltz
*)

let lwt = Conf.with_pkg "lwt"

let build_cmd c os files =
  OS.Cmd.run
  Cmd.(
    Pkg.build_cmd c os %%
    v "-plugin-tag" %% v "package(cppo_ocamlbuild)" %%
    Pkg.ocb_bool_tag c lwt "cppo_D(WITH_LWT)" %%
    of_list files
  )

let build = Pkg.build ~cmd:build_cmd ()
let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["cppo"]) "opam"]
let distrib =
  let watermarks = ("CODENAME", `String codename) :: Pkg.watermarks in
  Pkg.distrib ~watermarks ()

let () =
  Pkg.describe ~opams ~build:(Pkg.build ~cmd:build_cmd ()) ~distrib "shcaml" @@ fun c ->
  Ok [ Pkg.mllib ~api:[] "lib/shcaml.mllib";
       Pkg.lib ~exts:(Exts.exts [".cmi"; ".cmti"]) "lib/shcaml";
       Pkg.mllib ~api:[] "lib/shcaml_top.mllib";
       Pkg.lib "lib/shcaml_top_init.ml";
   ]
