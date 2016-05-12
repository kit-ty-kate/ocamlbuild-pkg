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

module Install = struct
  let tr = function
    | (str, Some target) ->
        "  \"" ^ str ^ "\" {\"" ^ target ^ "\"}"
    | (str, None) ->
        "  \"" ^ str ^ "\""

  let tr files =
    List.map tr
      (List.filter
         (fun (x, _) -> Sys.file_exists (Pathname.pwd / x))
         files
      )

  let dispatcher prod lib bin = function
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
      List.flatten (List.map print_subpackage subpackages)
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
        schema.subpackages @ List.flatten (List.map aux schema.subpackages)
      in
      schema :: aux schema
    in
    let get_lib schema = schema.dir / schema.name in
    let meta = schema.dir / "META" in
    let libs =
      List.flatten
        (List.map
           (fun x -> List.map (fun ext -> "_build" / get_lib x ^ ext) lib_exts)
           packages
        )
    in
    let modules =
      List.flatten
        (List.map
           (fun x ->
              List.flatten
                (List.map
                   (fun m ->
                      List.map
                        (fun ext -> "_build" / schema.dir / m ^ ext)
                        mod_exts
                   )
                   x.modules
                )
           )
           packages
        )
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
    (List.map (fun x -> (x, None)) ("_build" / meta :: libs @ modules), f)

  let dispatcher_bin name =
    let f hook =
      if hook = Before_options then begin
        Options.targets @:= [name ^ ".native"];
      end;
    in
    ([("_build" / name ^ ".native", Some name)], f)

  let dispatcher pkgs =
    let aux (name, lib, bins) =
      let lib =
        match lib with
        | Some lib -> Some (dispatcher_lib lib)
        | None -> None
      in
      (name, lib, List.map dispatcher_bin bins)
    in
    let pkgs = List.map aux pkgs in
    begin fun hook ->
      List.iter
        (fun (name, lib, bins) ->
           let lib = match lib with
             | Some (lib, f) -> f hook; lib
             | None -> []
           in
           let bins =
             List.flatten (List.map (fun (bins, f) -> f hook; bins) bins)
           in
           let install = name ^ ".install" in
           Install.dispatcher install lib bins hook;
           if hook = Before_options then begin
             Options.targets @:= [install];
           end;
        )
        pkgs;
    end
end
