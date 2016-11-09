open Ocamlbuild_plugin

type dispatcher

module Dispatcher : sig
  val to_dispatcher : (hook -> unit) -> dispatcher

  val dispatch : dispatcher list -> unit
end

module Install : sig
  type file
  type files

  val file :
    ?check:[`Check | `Optional | `NoCheck] ->
    ?target:string ->
    Pathname.t ->
    file

  val files : string -> file list -> files

  val dispatcher : Pathname.t -> files list -> dispatcher
end

module Substs : sig
  val dispatcher :
    Pathname.t list ->
    (string * string) list ->
    dispatcher
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

  val dispatcher : Pathname.t -> t -> dispatcher
end

module Mllib : sig
  val dispatcher : Pathname.t -> Pathname.t list -> dispatcher
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
    ?files:Install.files list ->
    unit ->
    t

  val dispatcher : t -> dispatcher
end
