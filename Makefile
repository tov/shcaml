OB=ocamlbuild -use-ocamlfind
ODOCFLAGS=-docflags -colorize-code,-charset,utf8,-stars,-t,"Shcaml",-intro,doc/INDEX

.PHONY: lib top utop plugin postprocess docs cleandoc clean

lib:
	$(OB) shcaml.cma shcaml.cmxa
	$(OB) shcaml_top.cma shcaml_top.cmxa

top:
	$(OB) shtop.top

utop:
	$(OB) shutop.top

runtop: top
	rlwrap ./shtop.top -I _build

runutop: utop
	./shutop.top -I _build

plugin:
	$(OB) -cflags -I,+ocamldoc -package compiler-libs doc/plugin.cmxs

postprocess: doc/postprocess.ml
	$(OB) -package lambdasoup -no-links doc/postprocess.byte

docs: lib plugin postprocess doc/INDEX
	$(OB) $(ODOCFLAGS) doc/api.docdir/index.html
	_build/doc/postprocess.byte
	cp doc/style.css _build/doc/api.docdir/style.css

doc/INDEX: doc/make-index.sed doc/tutorial.ml
	sed -nf $^ > $@
	mkdir -p _build/doc/
	cp doc/INDEX _build/doc/INDEX

cleandoc:
	rm -rf doc/INDEX
	rm -rf api.docdir
	rm -rf _build/doc/api.docdir

clean: cleandoc
	$(OB) -clean
