

let () =
  dispatch (
    Pkg.dispatcher {
      Pkg.descr = "An ocamlbuild plugin that helps packaging softwares";
      Pkg.version = "0.1";
      Pkg.requires = [];
      Pkg.name = "ocamlbuild-pkg";
      Pkg.dir = "src";
      Pkg.modules = [
        "ocamlbuild_plugin";
      ];
      Pkg.private_modules = [];
      Pkg.subpackages = [];
    };
  )
