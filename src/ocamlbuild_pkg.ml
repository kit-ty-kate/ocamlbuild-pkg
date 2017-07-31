module LazyMonad = Ocamlbuild_pkg_LazyMonad

open Ocamlbuild_plugin
open LazyMonad.Operator

module Options = struct
  let supports_native = LazyMonad.return begin fun () ->
    !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir / "libasmrun" -.- !Options.ext_lib
    |> Sys.file_exists
  end

  let supports_dynlink = LazyMonad.return begin fun () ->
    !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir / "dynlink.cmxa"
    |> Sys.file_exists
  end

  let (tr_build, init_build_dir) =
    let r = ref (lazy (assert false)) in
    let get file = LazyMonad.return (fun () -> Lazy.force !r / file) in
    let set x = r := Lazy.from_val x in
    (get, set)

  let ext_lib = LazyMonad.return (fun () -> !Options.ext_lib)
  let ext_obj = LazyMonad.return (fun () -> !Options.ext_obj)
  let build_dir = LazyMonad.return (fun () -> !Options.build_dir)
  let exe = LazyMonad.return (fun () -> !Options.exe)

  let targets = Options.targets
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
      Options.supports_native >|= begin function
      | true -> `Native
      | false -> fail "Native backend isn't supported by your architecture"
      end
  | Some `Byte -> LazyMonad.ret `Byte
  | None ->
      Options.supports_native >|= begin function
      | true -> `Native
      | false -> `Byte
      end

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
  Options.ext_lib >>= fun ext_lib ->
  Options.supports_dynlink >|= fun supports_dynlink ->
  let base_native = ext_lib :: "cmxa" :: base in
  match backend with
  | `Native when supports_dynlink -> "cmxs" :: base_native
  | `Native -> base_native
  | `Byte -> base

let mod_exts backend =
  let base = ["mli"; "cmi"; "cmti"; "cmo"] in
  Options.ext_obj >|= fun ext_obj ->
  match backend with
  | `Native -> "cmx" :: ext_obj :: base
  | `Byte -> base

let mllib_exts =
  let base = ["mllib"] in
  Options.supports_dynlink >|= function
  | true -> "mldylib" :: base
  | false -> base

let map_lib_exts backend file =
  lib_exts backend >|= List.map ((-.-) file)

let map_mod_exts backend file =
  mod_exts backend >|= List.map ((-.-) file)

let map_mllib_exts file =
  mllib_exts >|= List.map ((-.-) file)

module Install = struct
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
  type modul = Pathname.t

  let dispatcher name modules hook = match hook with
    | After_rules ->
        let aux prod = rule_file prod (fun _ -> (modules, [])) in
        let mllib_exts = LazyMonad.run hook (map_mllib_exts name) in
        List.iter aux mllib_exts;
    | _ ->
        ()
end

module Pkg = struct
  type modul = Pathname.t

  let capitalized_module modul =
    Pathname.dirname modul / String.capitalize (Pathname.basename modul)

  module Lib = struct
    type t = {
      name : string;
      dir : Pathname.t;
      modules : string list;
      private_modules : string list;
      backend : [`Native | `Byte] LazyMonad.t;
      subpackages : t list;
      mllib_packages : (Pathname.t * string list) list;
      meta : Pathname.t;
      meta_descr : META.t;
      files : Install.file list LazyMonad.t;
    }

    let create ~descr ~version ~requires ~name ~dir ~modules ?(private_modules=[]) ?backend ?(subpackages=[]) () =
      let backend = get_backend backend in
      let packages =
        let get_lib schema = schema.dir / schema.name in
        let rec aux schema =
          (get_lib schema, schema.modules, schema.private_modules) :: flat_map aux schema.subpackages
        in
        (dir / name, modules, private_modules) :: flat_map aux subpackages
      in
      let meta = dir / "META" in
      let meta_descr =
        let subpackages = List.map (fun x -> x.meta_descr) subpackages in
        META.create ~descr ~version ~requires ~name ~subpackages ()
      in
      let mllib_packages =
        List.map (fun (lib, m, pm) -> (lib, List.map capitalized_module (m @ pm))) packages
      in
      let files =
        backend >>= fun backend ->
        Options.tr_build meta >>= fun meta_file ->
        LazyMonad.List.float_map
          (fun (lib, _ , _) -> Options.tr_build lib >>= map_lib_exts backend)
          packages
        >>= fun libs ->
        LazyMonad.List.float_map
          (fun (_, modules, _) ->
             let aux m = Options.tr_build (dir / m) >>= map_mod_exts backend in
             LazyMonad.List.float_map aux modules
          )
          packages
        >|= fun modules ->
        List.map (Install.file ~check:`Check) (meta_file :: libs @ modules)
      in
      {
        name;
        dir;
        modules;
        private_modules;
        backend;
        subpackages;
        mllib_packages;
        meta;
        meta_descr;
        files;
      }

    let dispatcher {backend; mllib_packages; meta; meta_descr} hook =
      List.iter
        (fun (lib, modules) ->
           Mllib.dispatcher lib modules hook;
           if hook = After_options then begin
             let backend = LazyMonad.run hook backend in
             let mllib_exts = LazyMonad.run hook (map_mllib_exts lib) in
             let lib_exts = LazyMonad.run hook (map_lib_exts backend lib) in
             Options.targets @:= mllib_exts;
             Options.targets @:= lib_exts;
           end;
        )
        mllib_packages;
      META.dispatcher meta meta_descr hook;
      if hook = After_options then begin
        Options.targets @:= [meta];
      end
  end

  module Bin = struct
    type t = {
      main : Pathname.t;
      backend : [`Native | `Byte] LazyMonad.t;
      file : Install.file LazyMonad.t;
    }

    let ext_program = function
      | `Native -> "native"
      | `Byte -> "byte"

    let create ~main ?backend ?target () =
      let backend = get_backend backend in
      let target = get_target main target in
      let file =
        Options.exe >>= fun exe ->
        backend >>= fun backend ->
        let target = target ^ exe in
        Options.tr_build (main -.- ext_program backend) >|=
        Install.file ~check:`Check ~target
      in
      {
        main;
        backend;
        file;
      }

    let dispatcher {main; backend; file = _} hook = match hook with
      | After_options ->
          let backend = LazyMonad.run hook backend in
          Options.targets @:= [main -.- ext_program backend];
      | _ ->
          ()
  end

  type t = {
    eq : string -> bool;
    libs : Lib.t list;
    bins : Bin.t list;
    files : Install.dir list LazyMonad.t;
    install : Pathname.t;
  }

  let create ~name ?(libs=[]) ?(bins=[]) ?(files=[]) () =
    let install = name ^ ".install" in
    let files =
      LazyMonad.List.float_map (fun x -> x.Lib.files) libs >>= fun lib_files ->
      LazyMonad.List.map (fun x -> x.Bin.file) bins >|= fun bin_files ->
      ("lib", lib_files) :: ("bin", bin_files) :: files
    in
    let eq x = String.compare x name = 0 in
    {
      eq;
      libs;
      bins;
      files;
      install;
    }

  let dispatcher {eq; libs; bins; files; install} hook =
    if hook = After_options then begin
      let len_cwd = String.length Pathname.pwd in
      let opt_build_dir = LazyMonad.run hook Options.build_dir in
      let len_build_dir = String.length opt_build_dir in
      let new_build_dir =
        if len_build_dir >= len_cwd
        && String.compare (String.sub opt_build_dir 0 len_cwd) Pathname.pwd = 0 then begin
          String.sub opt_build_dir len_cwd (len_build_dir - len_cwd)
          |> (^) Pathname.current_dir_name
          |> Pathname.normalize
        end else begin
          opt_build_dir
        end
      in
      Options.init_build_dir new_build_dir;
      if List.exists eq !Options.targets then begin
        Options.targets := List.filter (fun x -> not (eq x)) !Options.targets;
        LazyMonad.eval hook files;
      end;
    end;
    if LazyMonad.is_val files then begin
      List.iter (fun x -> Lib.dispatcher x hook) libs;
      List.iter (fun x -> Bin.dispatcher x hook) bins;
      let files = LazyMonad.run hook files in
      Install.dispatcher install files hook;
      if hook = After_options then begin
        Options.targets @:= [install];
      end;
    end
end
