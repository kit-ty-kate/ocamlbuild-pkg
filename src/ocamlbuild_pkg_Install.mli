open Ocamlbuild_plugin

type file
type dir

val file :
  ?check:[`Check | `Optional] ->
  ?target:string ->
  Pathname.t ->
  file

val dir : dir:string -> file list -> dir

val dispatcher : Pathname.t -> dir list -> hook -> unit
