# Init OPAM
source .travis-opam-init.sh

# Check OPAM package description
opam lint

# Install
PKG=ocamlbuild-pkg
opam pin add -y --no-action --kind=git $PKG .
opam install -vy $PKG

# Run tests
make examples

# Uninstall
opam remove -vy $PKG
