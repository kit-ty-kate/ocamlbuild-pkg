open Ocamlbuild_plugin

type modul = Pathname.t

val dispatcher : Pathname.t -> modul list -> hook -> unit
