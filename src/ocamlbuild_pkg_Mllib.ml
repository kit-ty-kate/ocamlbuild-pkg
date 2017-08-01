open Ocamlbuild_plugin

module LazyMonad = Ocamlbuild_pkg_LazyMonad

open Ocamlbuild_pkg_Common

type modul = Pathname.t

let dispatcher name modules hook = match hook with
  | After_rules ->
      let aux prod = rule_file prod (fun _ -> (modules, [])) in
      let mllib_exts = LazyMonad.run hook (map_mllib_exts name) in
      List.iter aux mllib_exts;
  | _ ->
      ()
