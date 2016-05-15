all:
	cat src/ocamlbuild_pkg.ml bootstrap.ml > myocamlbuild.ml
	ocamlbuild -use-ocamlfind
	cp _build/ocamlbuild-pkg.install .

clean:
	rm -f myocamlbuild.ml
	ocamlbuild -clean
	rm -f ocamlbuild-pkg.install

.PHONY: all clean
