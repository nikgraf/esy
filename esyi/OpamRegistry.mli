type t

type pkg = {
  name: OpamManifest.PackageName.t;
  version: OpamVersion.Version.t;
  opam: Path.t;
  url: Path.t;
}

module Manifest : sig
  type t = private {
    name: OpamManifest.PackageName.t;
    version: OpamVersion.Version.t;
    opam : OpamFile.OPAM.t;
    url : OpamFile.URL.t option;
  }
end

val init : cfg:Config.t -> unit -> t RunAsync.t

val versions :
  t
  -> name : OpamManifest.PackageName.t
  -> (OpamVersion.Version.t * pkg) list RunAsync.t

val version :
    t
    -> name : OpamManifest.PackageName.t
    -> version : OpamVersion.Version.t
    -> Manifest.t option RunAsync.t
