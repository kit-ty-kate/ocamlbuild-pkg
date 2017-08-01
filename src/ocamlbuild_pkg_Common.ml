open Ocamlbuild_plugin

module LazyMonad = Ocamlbuild_pkg_LazyMonad
module Options = Ocamlbuild_pkg_Options

open LazyMonad.Operator

type backend = [`Native | `Byte]

let fail msg =
  Ocamlbuild_pack.Log.eprintf "Error: %s" msg;
  raise (Ocamlbuild_pack.My_std.Exit_with_code 1)

let get_backend = function
  | Some `Native ->
      Options.supports_native >|= begin function
      | true -> `Native
      | false -> fail "Native backend isn't supported by your architecture"
      end
  | Some `Byte -> LazyMonad.ret `Byte
  | None ->
      Options.supports_native >|= begin function
      | true -> `Native
      | false -> `Byte
      end

let get_target name = function
  | Some target -> target
  | None -> Filename.basename name

let rule_file file f =
  rule file ~prod:file
    (fun env _ ->
       let file = env file in
       let (content, commands) = f file in
       let content = String.concat "\n" content in
       Seq (Echo ([content ^ "\n"], file) :: commands)
    )

let lib_exts backend =
  let base = ["cma"] in
  Options.ext_lib >>= fun ext_lib ->
  Options.supports_dynlink >|= fun supports_dynlink ->
  let base_native = ext_lib :: "cmxa" :: base in
  match backend with
  | `Native when supports_dynlink -> "cmxs" :: base_native
  | `Native -> base_native
  | `Byte -> base

let mod_exts backend =
  let base = ["mli"; "cmi"; "cmti"; "cmo"] in
  Options.ext_obj >|= fun ext_obj ->
  match backend with
  | `Native -> "cmx" :: ext_obj :: base
  | `Byte -> base

let mllib_exts =
  let base = ["mllib"] in
  Options.supports_dynlink >|= function
  | true -> "mldylib" :: base
  | false -> base

let map_lib_exts backend file =
  lib_exts backend >|= List.map ((-.-) file)

let map_mod_exts backend file =
  mod_exts backend >|= List.map ((-.-) file)

let map_mllib_exts file =
  mllib_exts >|= List.map ((-.-) file)
