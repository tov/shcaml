open Ocamlbuild_plugin

let generate_INDEX env build =
  Cmd (Sh "sed -nf doc/make-index.sed doc/tutorial.ml > doc/INDEX")

let build_shcaml_doc env build =
  let generate_html =
    Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
      ~tags:["html"]
      ~ocamldoc:Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir
      "doc/api.odocl"
      "doc/api.docdir/index.html"
      "doc/api.docdir"
      env build
  in
  let postprocess_html = Cmd (A "doc/postprocess.byte") in
  Seq [generate_html; postprocess_html]


let () =
  rule "shcaml ocamldoc"
    ~insert:(`top)
    ~prod:"doc/api.docdir/index.html"
    ~stamp:"doc/api.docdir/html.stamp"
    ~deps:["doc/api.odocl"; "doc/postprocess.byte"; "doc/INDEX"]
    build_shcaml_doc;

  rule "shcaml INDEX"
    ~insert:(`top)
    ~prod:"doc/INDEX"
    ~deps:["doc/make-index.sed"; "doc/tutorial.ml"]
    generate_INDEX

let () = dispatch (fun hook ->
  begin match hook with
    | Before_options ->
      Options.ocaml_docflags := [
        "-colorize-code";
        "-charset"; "utf8";
        "-stars";
        "-t"; "Shcaml";
        "-intro"; "doc/INDEX"
      ]
    | _ -> ()
  end;
  Ocamlbuild_cppo.dispatcher hook
)
