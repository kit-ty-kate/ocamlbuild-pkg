open Ocamlbuild_plugin

module List = struct
  include List
  include Ocamlbuild_pkg_List
end

open Ocamlbuild_pkg_Common

type package = string

type t = {
  descr : string;
  version : string;
  requires : string list;
  name : string;
  subpackages : t list;
}

let create ~descr ~version ~requires ~name ~subpackages () = {
  descr;
  version;
  requires;
  name;
  subpackages;
}

let print_archive indent name =
  (indent ^ "archive(byte) = \"" ^ name ^ ".cma\"") ::
  (indent ^ "plugin(byte) = \"" ^ name ^ ".cma\"") ::
  (indent ^ "archive(native) = \"" ^ name ^ ".cmxa\"") ::
  (indent ^ "plugin(native) = \"" ^ name ^ ".cmxs\"") ::
  (indent ^ "exists_if = \"" ^ name ^ ".cma\"") ::
  []

let print_package =
  let rec aux indent {descr; version; requires; name; subpackages} =
    let print_subpackage package =
      (indent ^ "package \"" ^ package.name ^ "\" (") ::
      aux ("  " ^ indent) package @
      [")"]
    in
    (indent ^ "description = \"" ^ descr ^ "\"") ::
    (indent ^ "version = \"" ^ version ^ "\"") ::
    (indent ^ "requires = \"" ^ String.concat " " requires ^ "\"") ::
    print_archive indent name @
    List.flat_map print_subpackage subpackages
  in
  aux ""

let dispatcher prod package = function
  | After_rules ->
      rule_file prod (fun _ -> (print_package package, []));
  | _ ->
      ()
