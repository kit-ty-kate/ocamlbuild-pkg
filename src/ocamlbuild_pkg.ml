open Ocamlbuild_plugin

type dispatcher = hook -> unit

module Dispatcher = struct
  let to_dispatcher x = x

  let dispatch l = dispatch (fun hook -> List.iter (fun f -> f hook) l)
end

let supports_native = lazy begin
  !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir / "libasmrun" -.- !Options.ext_lib
  |> Sys.file_exists
end

let supports_dynlink = lazy begin
  !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir / "dynlink.cmxa"
  |> Sys.file_exists
end

let fail msg =
  Ocamlbuild_pack.Log.eprintf "Error: %s" msg;
  raise (Ocamlbuild_pack.My_std.Exit_with_code 1)

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

let assoc_and_destroy (k : string) l =
  let rec aux acc = function
    | (k', x)::xs when k = k' -> (x, List.rev_append acc xs)
    | x::xs -> aux (x :: acc) xs
    | [] -> raise Not_found
  in
  aux [] l

let split l =
  List.fold_left
    (fun (acc1, acc2) (x, y) -> (acc1 @ [x], acc2 @ [y]))
    ([], [])
    l

let split_map f l =
  split (List.map f l)

let get_backend = function
  | Some `Native ->
      if not !*supports_native then begin
        fail "Native backend isn't supported by your architecture"
      end;
      `Native
  | Some `Byte -> `Byte
  | None -> if !*supports_native then `Native else `Byte

let get_target name = function
  | Some target -> target
  | None -> Filename.basename name

let rule_file file f =
  rule file ~prod:file
    (fun env _ ->
       let file = env file in
       let (content, commands) = f file in
       let content = String.concat "\n" content in
       Seq (Echo ([content ^ "\n"], file) :: commands)
    )

let lib_exts backend =
  let base = ["cma"] in
  let base_native = !Options.ext_lib :: "cmxa" :: base in
  match backend with
  | `Native when !*supports_dynlink -> "cmxs" :: base_native
  | `Native -> base_native
  | `Byte -> base

let mod_exts backend =
  let base = ["mli"; "cmi"; "cmti"; "cmo"] in
  match backend with
  | `Native -> "cmx" :: !Options.ext_obj :: base
  | `Byte -> base

let mllib_exts = lazy begin
  let base = ["mllib"] in
  if !*supports_dynlink then "mldylib" :: base else base
end

let map_lib_exts file backend = List.map ((-.-) file) (lib_exts backend)
let map_mod_exts file backend = List.map ((-.-) file) (mod_exts backend)
let map_mllib_exts file = List.map ((-.-) file) !*mllib_exts

let build_dir = ref (lazy (assert false))

let tr_build file = Lazy.force !build_dir / file

module Install = struct
  type file = {
    file : Pathname.t;
    target : string option;
    check : [`Check | `Optional | `NoCheck];
  }

  type files = (string * file list)

  let file ?(check=`Check) ?target file = {
    file;
    target;
    check;
  }

  let files dir files = (dir, files)

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

  let dispatcher prod files = function
    | After_rules ->
        rule_file prod
          (fun prod ->
             let cmd = [A "ln"; A "-sf"; P (tr_build prod); Px Pathname.pwd] in
             (print files, [Cmd (S cmd)])
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
      flat_map print_subpackage subpackages
    in
    aux ""

  let dispatcher prod package = function
    | After_rules ->
        rule_file prod (fun _ -> (print_package package, []));
    | _ ->
        ()
end

module Mllib = struct
  let dispatcher name modules = function
    | After_rules ->
        let aux prod = rule_file prod (fun _ -> (modules, [])) in
        List.iter aux (map_mllib_exts name);
    | _ ->
        ()
end

module Pkg = struct
  let capitalized_module modul =
    Pathname.dirname modul / String.capitalize (Pathname.basename modul)

  module Lib = struct
    type t = {
      descr : string;
      version : string;
      requires : string list;
      name : string;
      dir : Pathname.t;
      modules : string list;
      private_modules : string list;
      backend : [`Native | `Byte] option;
      subpackages : t list;
    }

    let create ~descr ~version ~requires ~name ~dir ~modules ?(private_modules=[]) ?backend ?(subpackages=[]) () = {
      descr;
      version;
      requires;
      name;
      dir;
      modules;
      private_modules;
      backend;
      subpackages;
    }

    let rec to_meta_descr schema = {
      META.descr = schema.descr;
      META.version = schema.version;
      META.requires = schema.requires;
      META.name = schema.name;
      META.subpackages = List.map to_meta_descr schema.subpackages;
    }

    let dispatcher schema =
      let backend = get_backend schema.backend in
      let packages =
        let rec aux schema =
          schema.subpackages @ flat_map aux schema.subpackages
        in
        schema :: aux schema
      in
      let get_lib schema = schema.dir / schema.name in
      let meta = schema.dir / "META" in
      let libs =
        flat_map (fun x -> map_lib_exts (tr_build (get_lib x)) backend) packages
      in
      let modules =
        flat_map
          (fun x ->
             flat_map
               (fun m -> map_mod_exts (tr_build (schema.dir / m)) backend)
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
             if hook = After_options then begin
               Options.targets @:= (map_mllib_exts lib);
               Options.targets @:= (map_lib_exts lib backend);
             end;
          )
          mllib_packages;
        META.dispatcher meta meta_descr hook;
        if hook = After_options then begin
          Options.targets @:= [meta];
        end;
      in
      (List.map Install.file (tr_build meta :: libs @ modules), f)
  end

  module Bin = struct
    type t = {
      main : Pathname.t;
      backend : [`Native | `Byte] option;
      target : string option;
    }

    let create ~main ?backend ?target () = {
      main;
      backend;
      target;
    }

    let ext_program = function
      | `Native -> "native"
      | `Byte -> "byte"

    let dispatcher {main; backend; target} =
      let backend = get_backend backend in
      let target = get_target main target in
      let f hook =
        if hook = After_options then begin
          Options.targets @:= [main -.- ext_program backend];
        end;
      in
      let target = target ^ !Options.exe in
      (Install.file ~target (tr_build (main -.- ext_program backend)), f)
  end

  type t = {
    pkg_name : string;
    libs : Lib.t list;
    bins : Bin.t list;
    files : Install.files list;
  }

  let create ~name ?(libs=[]) ?(bins=[]) ?(files=[]) () = {
    pkg_name = name;
    libs;
    bins;
    files;
  }

  let dispatcher_aux {pkg_name; libs; bins; files} =
    let (libs, f_libs) = split_map Lib.dispatcher libs in
    let (bins, f_bins) = split_map Bin.dispatcher bins in
    let libs = List.flatten libs in
    let install = pkg_name ^ ".install" in
    let files = ("lib", libs) :: ("bin", bins) :: files in
    begin fun hook ->
      List.iter (fun f -> f hook) f_libs;
      List.iter (fun f -> f hook) f_bins;
      Install.dispatcher install files hook;
      if hook = After_options then begin
        Options.targets @:= [install];
      end;
    end

  let dispatcher pkg =
    let eq x = String.compare x pkg.pkg_name = 0 in
    let f = ref None in
    begin fun hook ->
      if hook = After_options then begin
        let len_cwd = String.length Pathname.pwd in
        let opt_build_dir = !Options.build_dir in
        let len_build_dir = String.length opt_build_dir in
        let new_build_dir =
          if len_build_dir >= len_cwd then begin
            let sub = String.sub opt_build_dir 0 len_cwd in
            if String.compare sub Pathname.pwd = 0 then begin
              String.sub opt_build_dir len_cwd (len_build_dir - len_cwd)
              |> (^) Pathname.current_dir_name
              |> Pathname.normalize
            end else begin
              opt_build_dir
            end
          end else begin
            opt_build_dir
          end
        in
        build_dir := lazy new_build_dir;
        if List.exists eq !Options.targets then begin
          Options.targets := List.filter (fun x -> not (eq x)) !Options.targets;
          f := Some (dispatcher_aux pkg)
        end;
      end;
      begin match !f with
      | Some f -> f hook
      | None -> ()
      end;
    end
end
