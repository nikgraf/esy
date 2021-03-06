module String = Astring.String
module Source = PackageInfo.Source

module Dist = struct
  type t = {
    name : string;
    version : PackageInfo.Version.t;
    source : PackageInfo.Source.t;
    tarballPath : Path.t option;
  }

  let pp fmt dist =
    Fmt.pf fmt "%s@%a" dist.name PackageInfo.Version.pp dist.version
end

let packageKey (pkg : Solution.Record.t) =
  let version = PackageInfo.Version.toString pkg.version in
  match pkg.opam with
  | None -> Printf.sprintf "%s__%s" pkg.name version
  | Some opam ->
    let opamHash =
      opam
      |> PackageInfo.OpamInfo.show
      |> Digest.string
      |> Digest.to_hex
      |> String.Sub.v ~start:0 ~stop:8
      |> String.Sub.to_string
    in
    Printf.sprintf "%s__%s__%s" pkg.name version opamHash

let fetch ~(cfg : Config.t) ({Solution.Record. name; version; source; opam; _} as record) =
  let open RunAsync.Syntax in

  let key = packageKey record in

  let doFetch path =
    match source with

    | PackageInfo.Source.LocalPath _ ->
      let msg = "Fetching " ^ name ^ ": NOT IMPLEMENTED" in
      failwith msg

    | PackageInfo.Source.LocalPathLink _ ->
      (* this case is handled separately *)
      return ()

    | PackageInfo.Source.NoSource ->
      return ()

    | PackageInfo.Source.Archive (url, _checksum)  ->
      let f tempPath =
        let%bind () = Fs.createDir tempPath in
        let tarballPath = Path.(tempPath / Filename.basename url) in
        let%bind () = Curl.download ~output:tarballPath url in
        let%bind () = Tarball.unpack ~stripComponents:1 ~dst:path tarballPath in
        return ()
      in
      Fs.withTempDir f

    | PackageInfo.Source.Github github ->
      let f tempPath =
        let%bind () = Fs.createDir tempPath in
        let tarballPath = Path.(tempPath / "package.tgz") in
        let%bind () =
          let url =
            Printf.sprintf
              "https://api.github.com/repos/%s/%s/tarball/%s"
              github.user github.repo github.commit
          in
          Curl.download ~output:tarballPath url
        in
        let%bind () =  Tarball.unpack ~stripComponents:1 ~dst:path tarballPath in
        return ()
      in
      Fs.withTempDir f

    | PackageInfo.Source.Git git ->
      let%bind () = Git.clone ~dst:path ~remote:git.remote () in
      let%bind () = Git.checkout ~ref:git.commit ~repo:path () in
      let%bind () = Fs.rmPath Path.(path / ".git") in
      return ()
    in

    let complete path =

      let removeEsyJsonIfExists () =
        let esyJson = Path.(path / "esy.json") in
        match%bind Fs.exists(esyJson) with
        | true -> Fs.unlink(esyJson)
        | false -> return ()
      in

      let%bind () =
        match opam with
        | Some {PackageInfo.OpamInfo. packageJson; files; patches} ->

          let%bind () = removeEsyJsonIfExists() in

          let%bind () =
            Fs.writeJsonFile ~json:packageJson Path.(path / "package.json")
          in

          let%bind () =
            let f (relPath, data) =
              let name = Path.append path relPath in
              let dirname = Path.parent name in
              let%bind () = Fs.createDir dirname in
              (* TODO: move this to the place we read data from *)
              let data =
                if String.get data (String.length data - 1) == '\n'
                then data
                else data ^ "\n"
              in
              let%bind () = Fs.writeFile ~data name in
              return()
            in
            List.map ~f files |> RunAsync.List.waitAll
          in

          let%bind() =
            let f patch =
              let patch = Path.(path / patch) in
              Patch.apply ~strip:1 ~root:path ~patch ()
            in RunAsync.List.processSeq ~f patches
          in
          return()

        | None -> return ()

      in

      let%bind () =
        let addResolvedFieldToPackageJson filename =
          match%bind Fs.readJsonFile filename with
          | `Assoc items ->
            let json = `Assoc (("_resolved", `String key)::items) in
            let data = Yojson.Safe.pretty_to_string json in
            Fs.writeFile ~data filename
          | _ -> error "invalid package.json"
        in

        let esyJson = Path.(path / "esy.json") in
        let packageJson = Path.(path / "package.json") in
        if%bind Fs.exists esyJson
        then addResolvedFieldToPackageJson esyJson
        else if%bind Fs.exists packageJson
        then addResolvedFieldToPackageJson packageJson
        else return ()
      in

      return ()

    in

    let tarballPath = Path.(cfg.tarballCachePath // v key |> addExt "tgz") in

    let dist = {Dist. tarballPath = Some tarballPath; name; version; source} in
    let%bind tarballIsInCache = Fs.exists tarballPath in

    match source, tarballIsInCache with
    | Source.LocalPathLink _, _ ->
      return dist

    | _, true ->
      return dist
    | _, false ->
      Fs.withTempDir (fun sourcePath ->
        let%bind () =
          let msg = Format.asprintf "fetching %a" PackageInfo.Source.pp source in
          RunAsync.withContext msg (
            let%bind () = Fs.createDir sourcePath in
            let%bind () = doFetch sourcePath in
            let%bind () = complete sourcePath in
            return ()
          )
        in

        let%bind () =
          let%bind () = Fs.createDir (Path.parent tarballPath) in
          let tempTarballPath = Path.(tarballPath |> addExt ".tmp") in
          let%bind () = Tarball.create ~filename:tempTarballPath sourcePath in
          let%bind () = Fs.rename ~src:tempTarballPath tarballPath in
          return ()
        in

        return dist
      )

let install ~cfg:_ ~path dist =
  let open RunAsync.Syntax in
  let {Dist. tarballPath; source; _} = dist in
  match source, tarballPath with

  | Source.LocalPathLink orig, _ ->
    let%bind () = Fs.createDir path in
    let%bind () =
      let data = (Path.toString orig) ^ "\n" in
      Fs.writeFile ~data Path.(path / "_esylink")
    in
    return ()

  | _, Some tarballPath ->
    let%bind () = Fs.createDir path in
    let%bind () = Tarball.unpack ~dst:path tarballPath in
    return ()
  | _, None ->
    return ()
