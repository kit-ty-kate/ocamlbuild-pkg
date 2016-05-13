open Ocamlbuild_plugin

let rule_file file f =
  rule file ~prod:file
    (fun env _ ->
       let file = env file in
       let content = f file in
       let content = String.concat "\n" content in
       Echo ([content ^ "\n"], file)
    )

let lib_exts = [".cma"; ".a"; ".cmxa"; ".cmxs"]
let mod_exts = [".mli"; ".cmi"; ".cmti"; ".cmo"; ".cmx"]

let map_lib_exts file = List.map (fun ext -> file ^ ext) lib_exts
let map_mod_exts file = List.map (fun ext -> file ^ ext) mod_exts

let tr_build file = "_build" / file

let flat_map f l = List.flatten (List.map f l)

module Install = struct
  type file = {
    file : Pathname.t;
    target : string option;
    check : [`Check | `Optional | `NoCheck];
  }

  let file ?(check=`Check) ?target file = {
    file;
    target;
    check;
  }

  let tr files =
    let aux {file; target; check} =
      if check = `Check && not (Sys.file_exists (Pathname.pwd / file)) then
        None
      else
        let file = if check = `Optional then "?" ^ file else file in
        let file = "  \"" ^ file ^ "\"" in
        let file = match target with
          | Some target -> file ^ " {\"" ^ target ^ "\"}"
          | None -> file
        in
        Some file
    in
    List.filter_opt aux files

  let dispatcher prod ~lib ~bin = function
    | After_rules ->
        rule_file prod
          (fun _ ->
             "lib: [" ::
             tr lib @
             ["]"] @
             "bin: [" ::
             tr bin @
             ["]"]
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
  type t = {
    descr : string;
    version : string;
    requires : string list;
    name : string;
    subpackages : t list;
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
      flat_map print_subpackage subpackages
    in
    aux ""

  let dispatcher prod package = function
    | After_rules ->
        rule_file prod (fun _ -> print_package package);
    | _ ->
        ()
end

module Mllib = struct
  let dispatcher name modules = function
    | After_rules ->
        let aux prod = rule_file prod (fun _ -> modules) in
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

  let capitalized_module modul =
    Pathname.dirname modul / String.capitalize (Pathname.basename modul)

  let rec to_meta_descr schema = {
    META.descr = schema.descr;
    META.version = schema.version;
    META.requires = schema.requires;
    META.name = schema.name;
    META.subpackages = List.map to_meta_descr schema.subpackages;
  }

  let dispatcher_lib schema =
    let packages =
      let rec aux schema =
        schema.subpackages @ flat_map aux schema.subpackages
      in
      schema :: aux schema
    in
    let get_lib schema = schema.dir / schema.name in
    let meta = schema.dir / "META" in
    let libs =
      flat_map (fun x -> map_lib_exts (tr_build (get_lib x))) packages
    in
    let modules =
      flat_map
        (fun x ->
           flat_map
             (fun m -> map_mod_exts (tr_build (schema.dir / m)))
             x.modules
        )
        packages
    in
    let meta_descr = to_meta_descr schema in
    let mllib_packages =
      List.map
        (fun schema ->
           let modules = schema.modules @ schema.private_modules in
           (get_lib schema, List.map capitalized_module modules)
        )
        packages
    in
    let f hook =
      List.iter
        (fun (lib, modules) ->
           Mllib.dispatcher lib modules hook;
           if hook = Before_options then begin
             Options.targets @:= [lib ^ ".mllib"];
             Options.targets @:= [lib ^ ".mldylib"];
             Options.targets @:= [lib ^ ".cma"];
             Options.targets @:= [lib ^ ".a"];
             Options.targets @:= [lib ^ ".cmxa"];
             Options.targets @:= [lib ^ ".cmxs"];
           end;
        )
        mllib_packages;
      META.dispatcher meta meta_descr hook;
      if hook = Before_options then begin
        Options.targets @:= [meta];
      end;
    in
    (List.map Install.file (tr_build meta :: libs @ modules), f)

  let dispatcher_bin target =
    let f hook =
      if hook = Before_options then begin
        Options.targets @:= [target ^ ".native"];
      end;
    in
    (Install.file ~target (tr_build (target ^ ".native")), f)

  let dispatcher {pkg_name; lib; bins} =
    let lib =
      match lib with
      | Some lib -> Some (dispatcher_lib lib)
      | None -> None
    in
    let bins = List.map dispatcher_bin bins in
    begin fun hook ->
      let lib = match lib with
        | Some (lib, f) -> f hook; lib
        | None -> []
      in
      let bin = List.map (fun (bin, f) -> f hook; bin) bins in
      let install = pkg_name ^ ".install" in
      Install.dispatcher install ~lib ~bin hook;
      if hook = Before_options then begin
        Options.targets @:= [install];
      end;
    end
end
