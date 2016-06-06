EXAMPLE_DIRS = \
        full \

all:
	cat src/ocamlbuild_pkg.ml bootstrap.ml > myocamlbuild.ml
	ocamlbuild -use-ocamlfind ocamlbuild-pkg

clean: examples-clean
	rm -f myocamlbuild.ml
	ocamlbuild -clean

examples:
	$(foreach d, $(EXAMPLE_DIRS), $(MAKE) -C examples/$(d);)

examples-clean:
	$(foreach d, $(EXAMPLE_DIRS), $(MAKE) -C examples/$(d) clean;)

.PHONY: all clean examples examples-clean
