open Ocamlbuild_plugin

val dispatcher :
  Pathname.t list ->
  (string * string) list ->
  hook ->
  unit
