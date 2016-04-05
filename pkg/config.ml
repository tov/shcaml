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
      "VERSION", "0.2.0";
      "CODENAME", "Shmaltz";

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

      "BUILDTIME", time; ]
end
