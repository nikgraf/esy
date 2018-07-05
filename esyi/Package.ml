module Req = PackageInfo.Req
module Version = PackageInfo.Version
module VersionSpec = PackageInfo.VersionSpec
module Source = PackageInfo.Source
module SourceSpec = PackageInfo.SourceSpec

type 'a conj = 'a list [@@deriving to_yojson]
type 'a disj = 'a list [@@deriving to_yojson]

module Deps = struct

  type t = req conj [@@deriving to_yojson]

  and req =
    | Npm of {name : string; formula : SemverVersion.Formula.DNF.t}
    | Opam of OpamFormula.t
    | Source of {name : string; spec : SourceSpec.t}

  let map ~f deps = List.map ~f:(List.map ~f) deps
  let filter ~f deps = List.map ~f:(List.filter ~f) deps

  let pp = Fmt.(vbox (list (list ~sep:cut Dep.pp)))
  let show v = Format.asprintf "%a" pp v

end

module Resolutions = struct
  module String = Astring.String

  type t = Version.t StringMap.t

  let empty = StringMap.empty

  let find resolutions pkgName =
    StringMap.find_opt pkgName resolutions

  let entries = StringMap.bindings

  let to_yojson v =
    let items =
      let f k v items = (k, (`String (Version.toString v)))::items in
      StringMap.fold f v []
    in
    `Assoc items

  let of_yojson =
    let open Result.Syntax in
    let parseKey k =
      match PackagePath.parse k with
      | Ok ((_path, name)) -> Ok name
      | Error err -> Error err
    in
    let parseValue key =
      function
      | `String v -> begin
        match String.cut ~sep:"/" key, String.cut ~sep:":" v with
        | Some ("@opam", _), Some("opam", _) -> Version.parse v
        | Some ("@opam", _), _ -> Version.parse ("opam:" ^ v)
        | _ -> Version.parse v
        end
      | _ -> Error "expected string"
    in
    function
    | `Assoc items ->
      let f res (key, json) =
        let%bind key = parseKey key in
        let%bind value = parseValue key json in
        Ok (StringMap.add key value res)
      in
      Result.List.foldLeft ~f ~init:empty items
    | _ -> Error "expected object"

  let apply resolutions (dep : Dep.t) =
    match find resolutions dep.name with
    | Some version ->
      let constr =
        match version with
        | Version.Npm version -> Constraint.Npm (SemverVersion.Formula.Constraint.EQ version)
        | Version.Opam version -> Constraint.Opam (OpamVersion.Formula.Constraint.EQ version)
        | Version.Source source -> Constraint.Source (SourceSpec.ofSource source)
      in
      Some {dep with constr}
    | None -> None

end


module BuildInfo = struct

  type t = {
    build : string list list;
    install : string list list;
    files : PackageInfo.File.t list;
    patches : PackageInfo.File.t list;
  } [@@deriving to_yojson]

end

type t = {
  name : string;
  version : PackageInfo.Version.t;
  source : PackageInfo.Source.t;
  dependencies: Deps.t;
  devDependencies: Deps.t;
  opam : PackageInfo.OpamInfo.t option;
  buildInfo : BuildInfo.t option;
  kind : kind;
} [@@deriving to_yojson]

and kind =
  | Esy
  | Npm

let pp fmt pkg =
  Fmt.pf fmt "%s@%a" pkg.name Version.pp pkg.version

let compare pkga pkgb =
  let name = String.compare pkga.name pkgb.name in
  if name = 0
  then Version.compare pkga.version pkgb.version
  else name

module Map = Map.Make(struct
  type nonrec t = t
  let compare = compare
end)

module Set = Set.Make(struct
  type nonrec t = t
  let compare = compare
end)
