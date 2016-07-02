

let () =
  Dispatcher.dispatch [
    Pkg.dispatcher {
      Pkg.pkg_name = "ocamlbuild-pkg";
      Pkg.libs = [
        {
          Pkg.Lib.descr = "An ocamlbuild plugin that helps packaging softwares";
          Pkg.Lib.version = "0.2+dev";
          Pkg.Lib.requires = ["ocamlbuild"];
          Pkg.Lib.name = "ocamlbuild-pkg";
          Pkg.Lib.dir = "src";
          Pkg.Lib.modules = [
            "ocamlbuild_pkg";
          ];
          Pkg.Lib.private_modules = [];
          Pkg.Lib.options = [];
          Pkg.Lib.subpackages = [];
        };
      ];
      Pkg.bins = [];
      Pkg.files = [];
    };
  ]
