open Ocamlbuild_plugin

module Install : sig
  type file
  type dir

  val file :
    ?check:[`Check | `Optional | `NoCheck] ->
    ?target:string ->
    Pathname.t ->
    file

  val dir : dir:string -> file list -> dir

  val dispatcher : Pathname.t -> dir list -> hook -> unit
end

module Substs : sig
  val dispatcher :
    Pathname.t list ->
    (string * string) list ->
    hook ->
    unit
end

module META : sig
  type t

  val create :
    descr:string ->
    version:string ->
    requires:string list ->
    name:string ->
    subpackages:t list ->
    unit ->
    t

  val dispatcher : Pathname.t -> t -> hook -> unit
end

module Mllib : sig
  val dispatcher : Pathname.t -> Pathname.t list -> hook -> unit
end

module Pkg : sig
  module Lib : sig
    type t

    val create :
      descr:string ->
      version:string ->
      requires:string list ->
      name:string ->
      dir:Pathname.t ->
      modules:string list ->
      ?private_modules:string list ->
      ?backend:[`Native | `Byte] ->
      ?subpackages : t list ->
      unit ->
      t
  end

  module Bin : sig
    type t

    val create :
      main:Pathname.t ->
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
    ?files:Install.dir list ->
    unit ->
    t

  val dispatcher : t -> hook -> unit
end
