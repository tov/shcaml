.PHONY: lib doc clean

lib:
	ocaml pkg/pkg.ml build

doc:
	topkg doc

clean:
	topkg clean
