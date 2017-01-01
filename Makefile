OB=ocamlbuild -use-ocamlfind
ODOCFLAGS=-docflags -colorize-code,-charset,utf8,-stars,-t,"Shcaml",-intro,doc/INDEX

.PHONY: lib plugin postprocess doc upload_doc cleandoc clean

lib:
	$(OB) shcaml.cma shcaml.cmxa
	$(OB) shcaml_top.cma shcaml_top.cmxa

plugin:
	$(OB) -cflags -I,+ocamldoc -package compiler-libs doc/plugin.cmxs

postprocess: doc/postprocess.ml
	$(OB) -package lambdasoup -no-links doc/postprocess.byte

doc: lib postprocess doc/INDEX
	$(OB) $(ODOCFLAGS) doc/api.docdir/index.html
	_build/doc/postprocess.byte
	cp doc/style.css _build/doc/api.docdir/style.css

doc/INDEX: doc/make-index.sed doc/tutorial.ml
	sed -nf $^ > $@
	mkdir -p _build/doc/
	cp doc/INDEX _build/doc/INDEX

upload_doc: doc
	git checkout gh-pages && rm -rf dev/* && cp api.docdir/* dev && \
	git add --all dev

cleandoc:
	rm -rf doc/INDEX
	rm -rf api.docdir
	rm -rf _build/doc/api.docdir

clean: cleandoc
	$(OB) -clean
