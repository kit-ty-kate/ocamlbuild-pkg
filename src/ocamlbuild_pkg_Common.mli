open Ocamlbuild_plugin

type backend = [`Native | `Byte]

val fail : ('a, unit, string, 'b) format4 -> 'a

val get_backend : backend option -> backend Ocamlbuild_pkg_LazyMonad.t
val get_target : Pathname.t -> Pathname.t option -> Pathname.t

val rule_file : Pathname.t -> (Pathname.t -> (string list * Command.t list)) -> unit

val lib_exts : backend -> string list Ocamlbuild_pkg_LazyMonad.t
val mod_exts : backend -> string list Ocamlbuild_pkg_LazyMonad.t
val mllib_exts : string list Ocamlbuild_pkg_LazyMonad.t

val map_lib_exts : backend -> Pathname.t -> Pathname.t list Ocamlbuild_pkg_LazyMonad.t
val map_mod_exts : backend -> Pathname.t -> Pathname.t list Ocamlbuild_pkg_LazyMonad.t
val map_mllib_exts : Pathname.t -> Pathname.t list Ocamlbuild_pkg_LazyMonad.t
