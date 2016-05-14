open Ocamlbuild_pkg

let () =
  Ocamlbuild_plugin.dispatch (
    Pkg.dispatcher {
      Pkg.pkg_name = "test-pkg";
      Pkg.lib = Some {
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
      Pkg.bins = [
      ];
    }
  )
