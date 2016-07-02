open Ocamlbuild_pkg

let () =
  Dispatcher.dispatch [
    Pkg.dispatcher {
      Pkg.pkg_name = "test-pkg";
      Pkg.libs = [
        {
          Pkg.Lib.descr = "Here is a description";
          Pkg.Lib.version = "0.1";
          Pkg.Lib.requires = [];
          Pkg.Lib.name = "test-pkg";
          Pkg.Lib.dir = "src";
          Pkg.Lib.options = [];
          Pkg.Lib.modules = [
            "test";
          ];
          Pkg.Lib.private_modules = [];
          Pkg.Lib.subpackages = [];
        };
      ];
      Pkg.bins = [];
      Pkg.files = [];
    }
  ]
