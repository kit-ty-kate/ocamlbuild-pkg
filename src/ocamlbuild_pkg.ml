open Ocamlbuild_plugin

let rule_file file f =
  rule file ~prod:file
    (fun env _ ->
       let file = env file in
       let content = f file in
       let content = String.concat "\n" content in
       Echo ([content ^ "\n"], file)
    )

module Install = struct
  let lib_exts = [".cma"; ".a"; ".cmxa"; ".cmxs"]
  let mod_exts = [".mli"; ".cmi"; ".cmti"; ".cmo"; ".cmx"]

  let tr str =
    "  \"" ^ str ^ "\""

  let tr_build str =
    "_build" / str

  let tr_ext str ext =
    tr_build (str ^ ext)

  let tr_exts exts str =
    List.map (tr_ext str) exts

  let tr_lib (libs, modules) =
    List.map tr
      (List.filter
         (fun x -> Sys.file_exists (Pathname.pwd / x))
         (List.flatten
            (List.map (tr_exts lib_exts) libs @
             List.map (tr_exts mod_exts) modules
            )
         )
      )

  let dispatcher prod meta lib = function
    | After_rules ->
        rule_file prod
          (fun _ ->
             "lib: [" ::
             tr (tr_build meta) ::
             tr_lib lib @
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
    dir : string;
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

  let dispatcher schema =
    let packages =
      let rec aux schema =
        schema.subpackages @ List.flatten (List.map aux schema.subpackages)
      in
      schema :: aux schema
    in
    let get_lib schema = schema.dir / schema.name in
    let meta = schema.dir / "META" in
    let install = schema.name ^ ".install" in
    let libs = List.map get_lib packages in
    let modules =
      List.flatten
        (List.map
           (fun x -> List.map (fun m -> schema.dir / m) x.modules)
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
    begin fun hook ->
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
      Install.dispatcher install meta (libs, modules) hook;
      if hook = Before_options then begin
        Options.targets @:= [install];
      end;
    end
end
