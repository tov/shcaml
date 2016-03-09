(* A very quiet formatter. *)
let silently = Format.make_formatter (fun _ _ _ -> ()) ignore

let () =
  Topdirs.dir_use silently "shcaml_top_init.ml";
  if !Sys.interactive
  then Printf.printf "\tCaml-Shcaml version %s (%s)\n\n%!"
      Version.version Version.version_name
