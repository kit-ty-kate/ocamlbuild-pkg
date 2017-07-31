open Ocamlbuild_plugin

module LazyMonad = Ocamlbuild_pkg_LazyMonad

let supports_native = LazyMonad.return begin fun () ->
  !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir / "libasmrun" -.- !Options.ext_lib
  |> Sys.file_exists
end

let supports_dynlink = LazyMonad.return begin fun () ->
  !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir / "dynlink.cmxa"
  |> Sys.file_exists
end

let (tr_build, init_build_dir) =
  let r = ref (lazy (assert false)) in
  let get file = LazyMonad.return (fun () -> Lazy.force !r / file) in
  let set x = r := Lazy.from_val x in
  (get, set)

let ext_lib = LazyMonad.return (fun () -> !Options.ext_lib)
let ext_obj = LazyMonad.return (fun () -> !Options.ext_obj)
let build_dir = LazyMonad.return (fun () -> !Options.build_dir)
let exe = LazyMonad.return (fun () -> !Options.exe)

let targets = Options.targets
