open Ocamlbuild_plugin

module Install : sig
  type file
  type files

  val file :
    ?check:[`Check | `Optional | `NoCheck] ->
    ?target:string ->
    Pathname.t ->
    file

  val files : string -> file list -> files

  val dispatcher : Pathname.t -> files list -> hook -> unit
end

module Substs : sig
  val dispatcher :
    Pathname.t list ->
    (string * string) list ->
    hook ->
    unit
end

module META : sig
  type t = {
    descr : string;
    version : string;
    requires : string list;
    name : string;
    subpackages : t list;
  }

  val dispatcher : Pathname.t -> t -> hook -> unit
end

module Mllib : sig
  val dispatcher : Pathname.t -> Pathname.t list -> hook -> unit
end

module Pkg : sig
  type lib = {
    descr : string;
    version : string;
    requires : string list;
    name : string;
    dir : Pathname.t;
    modules : string list;
    private_modules : string list;
    subpackages : lib list;
  }

  type t = {
    pkg_name : string;
    libs : lib list;
    bins : (Pathname.t * string option) list;
    files : Install.files list;
  }

  val dispatcher : t -> hook -> unit
end
