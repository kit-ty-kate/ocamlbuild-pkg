open Ocamlbuild_plugin

type t
type package = string

val create :
  descr:string ->
  version:string ->
  requires:package list ->
  name:string ->
  subpackages:t list ->
  unit ->
  t

val dispatcher : Pathname.t -> t -> hook -> unit
