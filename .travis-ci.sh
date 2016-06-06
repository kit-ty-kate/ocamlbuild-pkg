# Install OPAM
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sudo sh -s /usr/local/bin

# Init OPAM
opam init -y --compiler=$OCAML_VERSION
eval `opam config env`

# Check OPAM package description
opam lint

# Install
opam pin add -y --kind=git ocamlbuild-pkg .

# Run tests
make examples

# Uninstall
opam remove -y ocamlbuild-pkg
