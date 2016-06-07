open Ocamlbuild_pkg

let () =
  Dispatcher.dispatch [
    Pkg.dispatcher {
      Pkg.pkg_name = "test-pkg";
      Pkg.libs = [
        {
          Pkg.descr = "Here is a description";
          Pkg.version = "0.1";
          Pkg.requires = [];
          Pkg.name = "test-pkg";
          Pkg.dir = "src";
          Pkg.modules = [
            "test";
          ];
          Pkg.private_modules = [];
          Pkg.subpackages = [];
        };
      ];
      Pkg.bins = [];
      Pkg.files = [];
    }
  ]
