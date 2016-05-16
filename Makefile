all:
	cat src/ocamlbuild_pkg.ml bootstrap.ml > myocamlbuild.ml
	ocamlbuild -use-ocamlfind

clean:
	rm -f myocamlbuild.ml
	ocamlbuild -clean

.PHONY: all clean
