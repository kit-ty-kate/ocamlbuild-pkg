
let lib =
  Pkg.Lib.create
    ~descr:"An ocamlbuild plugin that helps packaging softwares"
    ~version:"0.2+dev"
    ~requires:["ocamlbuild"]
    ~name:"ocamlbuild-pkg"
    ~dir:"src"
    ~modules:[
      "ocamlbuild_pkg";
    ]
    ()

let ocamlbuild_pkg =
  Pkg.create
    ~name:"ocamlbuild-pkg"
    ~libs:[lib]
    ()

let () =
  Dispatcher.dispatch [
    Pkg.dispatcher ocamlbuild_pkg;
  ]
