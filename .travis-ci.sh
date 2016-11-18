# Init OPAM
source .travis-opam-init.sh

# Check OPAM package description
opam lint

# Install
opam pin add -y --no-action --kind=git ocamlbuild-pkg .
opam install -y ocamlbuild-pkg

# Run tests
make examples

# Uninstall
opam remove -y ocamlbuild-pkg
