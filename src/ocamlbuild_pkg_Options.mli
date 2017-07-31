open Ocamlbuild_plugin

val supports_native : bool Ocamlbuild_pkg_LazyMonad.t
val supports_dynlink : bool Ocamlbuild_pkg_LazyMonad.t

val tr_build : Pathname.t -> Pathname.t Ocamlbuild_pkg_LazyMonad.t
val init_build_dir : Pathname.t -> unit

val ext_lib : string Ocamlbuild_pkg_LazyMonad.t
val ext_obj : string Ocamlbuild_pkg_LazyMonad.t
val build_dir : Pathname.t Ocamlbuild_pkg_LazyMonad.t
val exe : string Ocamlbuild_pkg_LazyMonad.t

val targets : string list ref
