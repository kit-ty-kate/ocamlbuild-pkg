val flat_map : ('a -> 'b list) -> 'a list -> 'b list
val map_partition : ('a -> 'b option) -> 'a list -> 'b list * 'a list
val assoc_and_destroy : string -> (string * 'a) list -> 'a * (string * 'a) list
val split_map : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
