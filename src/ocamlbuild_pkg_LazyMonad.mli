open Ocamlbuild_plugin

type 'a t

val return : (unit -> 'a) -> 'a t
val ret : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : 'a t -> ('a -> 'b) -> 'b t

val run : hook -> 'a t -> 'a

val eval : hook -> _ t -> unit
val is_val : _ t -> bool

module Operator : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
end

module List : sig
  val map : ('a -> 'b t) -> 'a list -> 'b list t

  val float_map : ('a -> 'b list t) -> 'a list -> 'b list t
end
