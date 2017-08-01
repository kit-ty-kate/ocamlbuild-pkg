open Ocamlbuild_pkg

let lib =
  Pkg.Lib.create
    ~descr:"An ocamlbuild plugin that helps packaging softwares"
    ~version:"0.2.1"
    ~requires:["ocamlbuild"]
    ~name:"ocamlbuild-pkg"
    ~dir:"src"
    ~modules:[
      "ocamlbuild_pkg";
    ]
    ~private_modules:[
      "ocamlbuild_pkg_Pkg";
      "ocamlbuild_pkg_Install";
      "ocamlbuild_pkg_META";
      "ocamlbuild_pkg_Substs";
      "ocamlbuild_pkg_Mllib";
      "ocamlbuild_pkg_Common";
      "ocamlbuild_pkg_List";
      "ocamlbuild_pkg_LazyMonad";
      "ocamlbuild_pkg_Options";
    ]
    ()

let ocamlbuild_pkg =
  Pkg.create
    ~name:"ocamlbuild-pkg"
    ~libs:[lib]
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    Pkg.dispatcher ocamlbuild_pkg hook;
  )
