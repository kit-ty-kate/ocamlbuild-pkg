open Ocamlbuild_plugin

module Install = struct
  let lib_exts = [".cma"; ".a"; ".cmxa"; ".cmxs"]

  (* TODO: .cmti *)
  let mod_exts = [".mli"; ".cmi"; ".cmo"; ".cmx"]

  let tr str =
    "  \"" ^ str ^ "\""

  let tr_build dir str =
    tr ("_build" / dir / str)

  let tr_ext dir str ext =
    tr_build dir (str ^ ext)

  let tr_exts dir exts str =
    List.map (tr_ext dir str) exts

  let tr_lib dir (libs, modules) =
    List.flatten (List.map (tr_exts dir lib_exts) libs) @
    List.fold_left (fun acc str -> acc @ tr_exts dir mod_exts str) [] modules

  let dispatcher prod meta lib = function
    | After_rules ->
        rule prod
          ~prod
          (fun env _ ->
             let prod = env prod in
             let dir = Pathname.dirname prod in
             let content =
               "lib: [" ::
               tr meta ::
               tr_lib dir lib @
               ["]"]
             in
             let content = List.map (fun x -> x ^ "\n") content in
             Echo (content, prod)
          );
    | _ ->
        ()
end

module Substs = struct
  let dispatcher files substs = function
    | After_rules ->
        let aux file =
          rule (file ^ ".in -> " ^ file)
            ~dep:(file ^ ".in")
            ~prod:file
            (fun env _ ->
               let dep = env (file ^ ".in") in
               let prod = env file in
               let content = read_file dep in
               let content =
                 List.fold_left
                   (fun content (pat, repl) -> String.subst pat repl content)
                   content
                   substs
               in
               Echo ([content], prod)
            )
        in
        List.iter aux files;
    | _ ->
        ()
end

module META = struct
  type pkg = {
    descr : string;
    version : string;
    requires : string list;
    name : string;
    subpackages : pkg list;
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
      List.fold_left (fun acc pkg -> acc @ print_subpackage pkg) [] subpackages
    in
    aux ""

  let dispatcher prod package = function
    | After_rules ->
        rule prod
          ~prod
          (fun env _ ->
             let prod = env prod in
             let content = print_package package in
             let content = List.map (fun x -> x ^ "\n") content in
             Echo (content, prod)
          );
    | _ ->
        ()
end

module Mllib = struct
  let dispatcher name modules = function
    | After_rules ->
        let aux prod =
          rule prod
            ~prod
            (fun env _ ->
               let prod = env prod in
               let content = List.map (fun x -> x ^ "\n") modules in
               Echo (content, prod)
            )
        in
        aux (name ^ ".mllib");
        aux (name ^ ".mldylib");
    | _ ->
        ()
end

module Pkg = struct
  type t = {
    descr : string;
    version : string;
    requires : string list;
    name : string;
    dir : string;
    modules : string list;
    private_modules : string list;
    subpackages : t list;
  }

  let rec capitalized_module modul =
    Pathname.dirname modul / String.capitalize (Pathname.basename modul)

  let rec to_meta_descr schema = {
    META.descr = schema.descr;
    META.version = schema.version;
    META.requires = schema.requires;
    META.name = schema.name;
    META.subpackages = List.map to_meta_descr schema.subpackages;
  }

  let dispatcher schema hook = begin
    let packages =
      let rec aux schema =
        schema.subpackages @ List.flatten (List.map aux schema.subpackages)
      in
      schema :: aux schema
    in
    let get_lib schema = schema.dir / schema.name in
    let meta = schema.dir / "META" in
    let install = schema.name ^ ".install" in
    List.iter
      (fun schema ->
         let lib = get_lib schema in
         Mllib.dispatcher
           lib
           (List.map capitalized_module (schema.modules @ schema.private_modules))
           hook;
         Options.targets @:= [lib ^ ".mllib"];
         Options.targets @:= [lib ^ ".mldylib"];
         Options.targets @:= [lib ^ ".cma"];
         Options.targets @:= [lib ^ ".a"];
         Options.targets @:= [lib ^ ".cmxa"];
         Options.targets @:= [lib ^ ".cmxs"];
      )
      packages;
    META.dispatcher
      meta
      (to_meta_descr schema)
      hook;
    Options.targets @:= [meta];
    Install.dispatcher
      install
      meta
      (List.map get_lib packages, List.flatten (List.map (fun x -> x.modules) packages))
      hook;
    Options.targets @:= [install];
  end
end
