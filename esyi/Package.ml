module Version = PackageInfo.Version
module Source = PackageInfo.Source
module Dependencies = PackageInfo.Dependencies

module BuildInfo = struct

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
  dependencies: (Dependencies.t [@default Dependencies.empty]);
  devDependencies: (Dependencies.t [@default Dependencies.empty]);
  opam : PackageInfo.OpamInfo.t option;
  buildInfo : BuildInfo.t option;
  kind : kind;
}

and kind =
  | Esy
  | Npm

let ofOpamManifest ?name ?version (manifest : OpamManifest.t) =
  let open Run.Syntax in
  let name =
    match name with
    | Some name -> name
    | None -> OpamManifest.PackageName.toNpm manifest.name
  in
  let version =
    match version with
    | Some version -> version
    | None -> Version.Opam manifest.version
  in
  let source =
    match version with
    | Version.Source src -> src
    | _ -> manifest.source
  in
  return {
    name;
    version;
    dependencies = manifest.dependencies;
    devDependencies = manifest.devDependencies;
    source;
    opam = Some (OpamManifest.toPackageJson manifest version);
    buildInfo = None;
    kind = Esy;
  }

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
  return {
    name;
    version;
    dependencies = manifest.dependencies;
    devDependencies = manifest.devDependencies;
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

  let env _kind _id = Some (OpamVariable.bool true) in

  let depends = OpamFile.OPAM.depends opam in

  
  let _dependencies =
    let formula =
      OpamFilter.filter_formula (env `regular) depends
      [@ocaml.ppwarning "s"]
  in
    let dnf = OpamFormula.to_dnf formula in
    let f map conj =
      let f map ((name, _constr) : OpamFormula.atom) =
        let name = OpamPackage.Name.to_string name in
        let name = "@opam/" ^ name in
        match StringMap.find_opt name map with
        | Some _formula -> map
        | None -> map
      in
      List.fold_left ~f ~init:map conj
    in
    List.fold_left ~f ~init:StringMap.empty dnf
  in

  return {
    name;
    version;
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
