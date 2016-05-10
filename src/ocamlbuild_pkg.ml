open Ocamlbuild_plugin

let rule_file file f =
  rule file ~prod:file
    (fun env _ ->
       let file = env file in
       let content = f file in
       let content = String.concat "\n" content in
       Echo ([content], file)
    )

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
    List.flatten (List.map (tr_exts dir mod_exts) modules)

  let dispatcher prod meta lib = function
    | After_rules ->
        rule_file prod
          (fun prod ->
             let dir = Pathname.dirname prod in
             "lib: [" ::
             tr_build dir meta ::
             tr_lib dir lib @
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
      List.fold_left (fun acc pkg -> acc @ print_subpackage pkg) [] subpackages
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
         if hook = Before_options then begin
           Options.targets @:= [lib ^ ".mllib"];
           Options.targets @:= [lib ^ ".mldylib"];
           Options.targets @:= [lib ^ ".cma"];
           Options.targets @:= [lib ^ ".a"];
           Options.targets @:= [lib ^ ".cmxa"];
           Options.targets @:= [lib ^ ".cmxs"];
         end;
      )
      packages;
    META.dispatcher
      meta
      (to_meta_descr schema)
      hook;
    if hook = Before_options then begin
      Options.targets @:= [meta];
    end;
    Install.dispatcher
      install
      meta
      (List.map get_lib packages, List.flatten (List.map (fun x -> x.modules) packages))
      hook;
    if hook = Before_options then begin
      Options.targets @:= [install];
    end;
  end
end
