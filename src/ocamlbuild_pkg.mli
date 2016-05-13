open Ocamlbuild_plugin

module Install : sig
  type file

  val file :
    ?check:[`Check | `Optional | `NoCheck] ->
    ?target:string ->
    Pathname.t ->
    file

  val dispatcher :
    Pathname.t ->
    lib:file list ->
    bin:file list ->
    hook ->
    unit
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

  val dispatcher :
    Pathname.t ->
    t ->
    hook ->
    unit
end

module Mllib : sig
  val dispatcher :
    Pathname.t ->
    Pathname.t list ->
    hook ->
    unit
end

module Pkg : sig
  type t = {
    descr : string;
    version : string;
    requires : string list;
    name : string;
    dir : Pathname.t;
    modules : string list;
    private_modules : string list;
    subpackages : t list;
  }

  type pkg = {
    pkg_name : string;
    lib : t option;
    bins : Pathname.t list;
  }

  val dispatcher :
    pkg ->
    hook ->
    unit
end