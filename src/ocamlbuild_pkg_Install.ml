open Ocamlbuild_plugin

module LazyMonad = Ocamlbuild_pkg_LazyMonad
module Options = Ocamlbuild_pkg_Options

open Ocamlbuild_pkg_Common

type file = {
  file : Pathname.t;
  target : string option;
  check : [`Check | `Optional] option;
}

type dir = (string * file list)

let file ?check ?target file = {
  file;
  target;
  check;
}

let dir ~dir files = (dir, files)

let tr files =
  let aux {file; target; check} =
    if check = Some `Check && not (Sys.file_exists (Pathname.pwd / file)) then
      None
    else
      let file = if check = Some `Optional then "?" ^ file else file in
      let file = "  \"" ^ file ^ "\"" in
      let file = match target with
        | Some target -> file ^ " {\"" ^ target ^ "\"}"
        | None -> file
      in
      Some file
  in
  List.filter_opt aux files

let merge files =
  let rec aux acc = function
    | [] ->
        acc
    | (dir, files)::xs ->
        let eq (d, _) = String.compare dir d = 0 in
        let l = List.flatten (List.map snd (List.find_all eq xs)) in
        let xs = List.filter (fun x -> not (eq x)) xs in
        aux (acc @ [(dir, files @ l)]) xs
  in
  aux [] files

let print files =
  let aux = function
    | (_, []) ->
        []
    | (dir, files) ->
        (dir ^ ": [") ::
        tr files @
        ["]"]
  in
  List.flatten (List.map aux (merge files))

let dispatcher prod files hook = match hook with
  | After_rules ->
      rule_file prod
        (fun prod ->
           let prod_file = LazyMonad.run hook (Options.tr_build prod) in
           let cmd = [A "ln"; A "-sf"; P prod_file; Px Pathname.pwd] in
           (print files, [Cmd (S cmd)])
        );
  | _ ->
      ()
