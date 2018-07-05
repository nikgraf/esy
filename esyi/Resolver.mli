(** Resolution is a pair of a package name and a package version *)
module Resolution : sig
  type t = private {
    name: string;
    version: PackageInfo.Version.t
  }

  val pp : t Fmt.t
end

(** Package request resolver *)
type t

(** Make new resolver *)
val make : cfg:Config.t -> unit -> t RunAsync.t

(** Resolve package request into a list of resolutions *)
val resolve :
  name:string
  -> formula:Package.ConstraintFormula.t
  -> t
  -> (PackageInfo.Req.t * Resolution.t list) RunAsync.t

(** Fetch the package metadata given the resolution. *)
val package :
  resolution:Resolution.t
  -> t
  -> Package.t RunAsync.t
