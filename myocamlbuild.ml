open Ocamlbuild_plugin

let () =
  dispatch (function
    | After_options ->
      let ocamldoc tags deps docout docdir =
        let tags = tags -- "extension:html" in
        Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir in
        
      rule "ocamldoc with plugin"
        ~prod:"%.docdir/index.html"
        ~stamp:"%.docdir/html.stamp"
        ~dep:"%.odocl"
        (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
           ~ocamldoc "%.odocl" "%.docdir/index.html" "%.docdir");

      pflag ["ocaml"; "doc"; "docdir"] "plugin"
        (fun plugin -> S [A "-g"; A plugin]);

    | _ -> ())
