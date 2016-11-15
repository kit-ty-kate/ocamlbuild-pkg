open Ocamlbuild_plugin

module Install : sig
  type file
  type dir

  (** [file ?check ?target filename] represents a file [filename] to be installed.
      ?check ask whenever you want ocamlbuild-pkg to check if the file exists
             before adding it to the .install file or if you just want it to be
             optional.
      ?target ask you if you want the file to be renamed at installation *)
  val file :
    ?check:[`Check | `Optional] ->
    ?target:string ->
    Pathname.t ->
    file

  (** [dir ~dir files] reprensents a list of file to be installed in a given directory.
      To see the directory accepted, see the opam documentation *)
  val dir : dir:string -> file list -> dir

  (** [dispatcher install dirs hook] creates a rule that create an [install] file
      (usually suffixed by .install) with [dirs] as content. *)
  val dispatcher : Pathname.t -> dir list -> hook -> unit
end

module Substs : sig
  (** [dispatcher files substs hook] substitutes for each file [files].in each pain key/value
      in [substs] and produces [files] in the build directory.
      Does nothing if the key doesn't exist. *)
  val dispatcher :
    Pathname.t list ->
    (string * string) list ->
    hook ->
    unit
end

module META : sig
  type t

  (** An ocamlfind package name *)
  type package = string

  (** [create ~descr ~version ~requires ~name ~subpackages ()] creates a META description
      with each parameters being a field. *)
  val create :
    descr:string ->
    version:string ->
    requires:package list ->
    name:string ->
    subpackages:t list ->
    unit ->
    t

  (** [dispatcher filename descr hook] creates a META file [filename] with [descr] as
      META description. *)
  val dispatcher : Pathname.t -> t -> hook -> unit
end

module Mllib : sig
  (** An OCaml module name (i.e. [some/path/Module] or just [Module]) *)
  type modul = Pathname.t

  (** [dispatcher filename modules hook] creates a [filename].mllib
      (and [filename].mldylib if available) filled with [modules]. *)
  val dispatcher : Pathname.t -> modul list -> hook -> unit
end

module Pkg : sig
  (** A module name (case sensitive corresponding to the file's case) without extension.
      i.e. [a/path/module], [module] or [some/upper/case/Module]. *)
  type modul = Pathname.t

  module Lib : sig
    type t

    (** [create ~descr ~version ~requires ~name ~dir ~modules
         ?private_modules ?backend ?subpackages ()]
        describes a library matching those parameters.
        [private_modules] is a list of modules that are not installed but just
        used during the compilation process. *)
    val create :
      descr:string ->
      version:string ->
      requires:META.package list ->
      name:string ->
      dir:Pathname.t ->
      modules:modul list ->
      ?private_modules:modul list ->
      ?backend:[`Native | `Byte] ->
      ?subpackages : t list ->
      unit ->
      t
  end

  module Bin : sig
    type t

    (** [create ~main ?backend ?target ()] describes an executable that has a
        [main] module and will be installed as [target]. *)
    val create :
      main:modul ->
      ?backend:[`Native | `Byte] ->
      ?target:string ->
      unit ->
      t
  end

  type t

  (** [create ~name ?libs ?bins ?files ()] describes a package to be compiled and installed. *)
  val create :
    name:string ->
    ?libs:Lib.t list ->
    ?bins:Bin.t list ->
    ?files:Install.dir list ->
    unit ->
    t

  (** [dispatcher pkg hook] creates everything that is needed for installing your package:
       - a META file for each [libs].
       - [name].install to be used with opam (or opam-install). [files] contains
         additional files to be installed.
      And also building the package:
       - a .mllib/.mldylib file for each [libs].
      To unable this function and to actually build the package, it is also
      required to call ocamlbuild with the package name as argument. For instance:
       [ocamalbuild -use-ocamlfind -plugin-tag "package(ocamlbuild-pkg)" your-pkg-name] *)
  val dispatcher : t -> hook -> unit
end
