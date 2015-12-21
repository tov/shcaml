#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default

  let vars =
    let time = Cmd.read "date" |> function
    | `Ok t -> t
    | `Error _ -> "" in

    [ "NAME", "shcaml";
      "VERSION", "0.1.3";
      "CODENAME", "Shmeer";
      "BUILDTIME", time; ]
end
