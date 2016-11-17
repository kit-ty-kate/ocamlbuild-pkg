# Init OPAM bin directory
mkdir -p $HOME/opam-bin
export PATH="$HOME/opam-bin:$PATH"

# Install & Init OPAM if not cached
if type opam; then
    cp -r $HOME/opam-cache $HOME/.opam
else
    # Install & Init OPAM
    wget "https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux" -O $HOME/opam-bin/opam
    chmod u+x $HOME/opam-bin/opam
    opam init -y --compiler=$OCAML_VERSION

    # Save for cache
    rm -rf $HOME/opam-cache
    cp -r $HOME/.opam $HOME/opam-cache
fi

eval `opam config env`

# Check OPAM package description
opam lint

# Install
opam pin add -y --no-action --kind=git ocamlbuild-pkg .
opam install -y ocamlbuild-pkg

# Run tests
make examples

# Uninstall
opam remove -y ocamlbuild-pkg
