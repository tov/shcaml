.PHONY: lib postprocess doc upload_doc cleandoc clean

lib:
	ocaml pkg/pkg.ml build

doc:
	topkg doc

upload_doc: doc
	git checkout gh-pages && rm -rf dev/* && cp api.docdir/* dev && \
	git add --all dev

cleandoc:
	rm -rf doc/INDEX
	rm -rf api.docdir
	rm -rf _build/doc/api.docdir

clean: cleandoc
	topkg clean
