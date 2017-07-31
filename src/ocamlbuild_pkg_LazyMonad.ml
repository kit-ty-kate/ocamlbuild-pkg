open Ocamlbuild_plugin

type 'a t = 'a Lazy.t

let return = Lazy.from_fun
let ret = Lazy.from_val
let bind x f = lazy (Lazy.force (f (Lazy.force x)))
let map x f = lazy (f (Lazy.force x))

let run hook x =
  match hook with
  | Before_options -> assert false
  | _ -> Lazy.force x

let eval hook x = ignore (run hook x)
let is_val = Lazy.is_val

module Operator = struct
  let (>>=) = bind
  let (>|=) = map
end

module List = struct
  open Operator

  let rec map f = function
    | [] -> ret []
    | x::xs ->
        f x >>= fun x ->
        map f xs >|= fun xs ->
        x :: xs

  let float_map f l = map f l >|= List.concat
end
