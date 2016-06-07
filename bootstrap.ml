

let () =
  Dispatcher.dispatch [
    Pkg.dispatcher {
      Pkg.pkg_name = "ocamlbuild-pkg";
      Pkg.libs = [
        {
          Pkg.descr = "An ocamlbuild plugin that helps packaging softwares";
          Pkg.version = "0.2+dev";
          Pkg.requires = ["ocamlbuild"];
          Pkg.name = "ocamlbuild-pkg";
          Pkg.dir = "src";
          Pkg.modules = [
            "ocamlbuild_pkg";
          ];
          Pkg.private_modules = [];
          Pkg.subpackages = [];
        };
      ];
      Pkg.bins = [];
      Pkg.files = [];
    }
  ]
