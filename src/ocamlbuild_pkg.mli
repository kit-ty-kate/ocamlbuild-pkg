open Ocamlbuild_plugin

module Install : sig
  val dispatcher :
    Pathname.t ->
    (Pathname.t * string option) list ->
    (Pathname.t * string option) list ->
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

  val dispatcher :
    (string * t option * Pathname.t list) list ->
    hook ->
    unit
end
