open Ocamlbuild_pkg

let lib =
  Pkg.Lib.create
    ~descr:"Here is a description"
    ~version:"0.1"
    ~requires:[]
    ~name:"test-pkg"
    ~dir:"src"
    ~modules:[
      "test";
    ]
    ()

let test_pkg =
  Pkg.create
    ~name:"test-pkg"
    ~libs:[lib]
    ()

let () =
  Dispatcher.dispatch [
    Pkg.dispatcher test_pkg;
  ]
