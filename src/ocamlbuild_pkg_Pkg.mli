open Ocamlbuild_plugin

type modul = Pathname.t

module Lib : sig
  type t

  val create :
    descr:string ->
    version:string ->
    requires:Ocamlbuild_pkg_META.package list ->
    name:string ->
    dir:Pathname.t ->
    modules:modul list ->
    ?private_modules:modul list ->
    ?backend:[`Native | `Byte] ->
    ?subpackages : t list ->
    unit ->
    t
end

module Bin : sig
  type t

  val create :
    main:modul ->
    ?backend:[`Native | `Byte] ->
    ?target:string ->
    unit ->
    t
end

type t

val create :
  name:string ->
  ?libs:Lib.t list ->
  ?bins:Bin.t list ->
  ?files:Ocamlbuild_pkg_Install.dir list ->
  unit ->
  t

val dispatcher : t -> hook -> unit
