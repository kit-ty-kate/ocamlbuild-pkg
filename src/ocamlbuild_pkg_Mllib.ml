open Ocamlbuild_plugin

module LazyMonad = Ocamlbuild_pkg_LazyMonad

open Ocamlbuild_pkg_Common

type modul = Pathname.t

let check_module_char name i = function
  | 'A'..'Z' when i = 0 -> ()
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' when i > 0 -> ()
  | _ -> fail "Incorrect module name '%s'" name

let check_module_name name =
  let name = Pathname.basename name in
  String.iteri (check_module_char name) name

let check_modules_names = List.iter check_module_name

let dispatcher name modules hook = match hook with
  | After_rules ->
      check_modules_names modules;
      let aux prod = rule_file prod (fun _ -> (modules, [])) in
      let mllib_exts = LazyMonad.run hook (map_mllib_exts name) in
      List.iter aux mllib_exts;
  | _ ->
      ()
