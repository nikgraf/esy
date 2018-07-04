module Req = PackageInfo.Req
module Version = PackageInfo.Version
module VersionSpec = PackageInfo.VersionSpec
module Source = PackageInfo.Source
module SourceSpec = PackageInfo.SourceSpec

module Dep = struct
  type t = name * constr [@@deriving to_yojson]

  and name = string

  and relop =
    | EQ
    | NEQ
    | LT
    | LTE
    | GT
    | GTE

  and version =
    | Npm of SemverVersion.Version.t
    | Opam of OpamVersion.Version.t

  and constr =
    | Rel of relop * version
    | Source of SourceSpec.t
end

module Deps = struct

  type t = Dep.t conj disj [@@deriving to_yojson]
  and 'a conj = 'a list
  and 'a disj = 'a list

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

(* let ofOpamManifest ?name ?version (manifest : OpamManifest.t) = *)
(*   let open Run.Syntax in *)
(*   let name = *)
(*     match name with *)
(*     | Some name -> name *)
(*     | None -> OpamManifest.PackageName.toNpm manifest.name *)
(*   in *)
(*   let version = *)
(*     match version with *)
(*     | Some version -> version *)
(*     | None -> Version.Opam manifest.version *)
(*   in *)
(*   let source = *)
(*     match version with *)
(*     | Version.Source src -> src *)
(*     | _ -> manifest.source *)
(*   in *)
(*   return { *)
(*     name; *)
(*     version; *)
(*     dependencies = manifest.dependencies; *)
(*     devDependencies = manifest.devDependencies; *)
(*     source; *)
(*     opam = Some (OpamManifest.toPackageJson manifest version); *)
(*     buildInfo = None; *)
(*     kind = Esy; *)
(*   } *)

let ofManifest ?name ?version (manifest : Manifest.t) =
  let open Run.Syntax in
  let name =
    match name with
    | Some name -> name
    | None -> manifest.name
  in
  let version =
    match version with
    | Some version -> version
    | None -> Version.Npm (SemverVersion.Version.parseExn manifest.version)
  in
  let source =
    match version with
    | Version.Source src -> src
    | _ -> manifest.source
  in
  let dependencies =
    manifest.dependencies
    |> PackageInfo.Dependencies.toList
    |> List
  in
  return {
    name;
    version;
    dependencies = [manifest.dependencies];
    devDependencies = [manifest.devDependencies];
    source;
    opam = None;
    buildInfo = None;
    kind =
      if manifest.hasEsyManifest
      then Esy
      else Npm
  }

let ofOpamFile ~name ~version (_url : OpamFile.URL.t option) (opam : OpamFile.OPAM.t) =
  let open Run.Syntax in

  let depends = OpamFile.OPAM.depends opam in

  let opamFormulaToDependencies formula =
    let module F = SemverVersion.Formula in
    let dnf = OpamFormula.to_dnf formula in
    let dependencies =
      let f reqs conj =
        let conjMap =
          let f map ((name, _constr) : OpamFormula.atom) =
            let name = OpamPackage.Name.to_string name in
            let name = "@opam/" ^ name in
            match StringMap.find_opt name map with
            | Some prevConstr ->
              let constr = F.Constraint.ANY::prevConstr in
              StringMap.add name constr map
            | None ->
              let constr = F.Constraint.ANY in
              StringMap.add name [constr] map
          in
          List.fold_left ~f ~init:StringMap.empty conj
        in
        let formulas =
          let f name constrs reqs =
            let formula = F.OR [F.AND constrs] in
            let spec = VersionSpec.Npm formula in
            let req = Req.ofSpec ~name ~spec in
            req::reqs
          in
          Dependencies.ofList (StringMap.fold f conjMap [])
        in
        formulas::reqs
      in
      List.fold_left ~f ~init:[] dnf
    in
    dependencies
  in

  let dependencies =
    let formula =
      OpamFilter.filter_deps
        ~build:true ~post:true ~test:false ~doc:false ~dev:false
        depends
    in opamFormulaToDependencies formula
  in

  let devDependencies =
    let formula =
      OpamFilter.filter_deps
        ~build:true ~post:true ~test:true ~doc:true ~dev:true
        depends
    in opamFormulaToDependencies formula
  in

  return {
    name;
    version;
    dependencies;
    devDependencies;
    kind = Esy;
    opam = None;
    source = PackageInfo.Source.NoSource;
    buildInfo = None;
  }

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
