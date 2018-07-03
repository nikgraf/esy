(**
 * This normalizes the info between different manifests types (opam and npm).
 *)

module BuildInfo : sig

  type t = {
    build : string list list;
    install : string list list;
    files : PackageInfo.File.t list;
    patches : PackageInfo.File.t list;
  }

end

type t = {
  name : string;
  version : PackageInfo.Version.t;
  source : PackageInfo.Source.t;
  dependencies: PackageInfo.Dependencies.t;
  devDependencies: PackageInfo.Dependencies.t;

  (* TODO: make it non specific to opam. *)
  opam : PackageInfo.OpamInfo.t option;

  buildInfo : BuildInfo.t option;
  kind : kind;
}

and kind =
  | Esy
  | Npm

(**
 * Make package out of opam manifest.
 *
 * Optional arguments `name` and `version` are used to override name and version
 * specified in manifest if needed.
 *)
val ofOpamManifest :
  ?name:string
  -> ?version:PackageInfo.Version.t
  -> OpamManifest.t
  -> t Run.t

val ofOpamFile :
  ?name:string
  -> ?version:PackageInfo.Version.t
  -> OpamFile.URL.t option
  -> OpamFile.OPAM.t
  -> t Run.t

(**
 * Make package out of package.json manifest.
 *
 * Optional arguments `name` and `version` are used to override name and version
 * specified in manifest if needed.
 *)
val ofManifest :
  ?name:string
  -> ?version:PackageInfo.Version.t
  -> Manifest.t
  -> t Run.t

val pp : t Fmt.t
val compare : t -> t -> int

module Map : Map.S with type key := t
module Set : Set.S with type elt := t
