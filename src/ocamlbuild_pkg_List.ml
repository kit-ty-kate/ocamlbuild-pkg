let flat_map f l = List.flatten (List.map f l)

let map_partition f l =
  let rec aux (acc1, acc2) = function
    | x::xs ->
        begin match f x with
        | Some x -> aux (x :: acc1, acc2) xs
        | None -> aux (acc1, x :: acc2) xs
        end
    | [] -> (List.rev acc1, List.rev acc2)
  in
  aux ([], []) l

let assoc_and_destroy k l =
  let rec aux acc = function
    | (k', x)::xs when String.equal k k' -> (x, List.rev_append acc xs)
    | x::xs -> aux (x :: acc) xs
    | [] -> raise Not_found
  in
  aux [] l

let split_map f l =
  List.split (List.map f l)
