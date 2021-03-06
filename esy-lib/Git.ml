
type ref = string
type commit = string
type remote = string

let runGit cmd =
  let f p =
    let%lwt stdout = Lwt_io.read p#stdout
    and stderr = Lwt_io.read p#stderr in
    match%lwt p#status with
    | Unix.WEXITED 0 ->
      RunAsync.return ()
    | _ ->
      Logs_lwt.err (fun m -> m
        "@[<v>command failed: %a@\nstderr:@[<v 2>@\n%a@]@\nstdout:@[<v 2>@\n%a@]@]"
        Cmd.pp cmd Fmt.lines stderr Fmt.lines stdout
      );%lwt
      RunAsync.error "error running command"
  in
  try%lwt
    let cmd = Cmd.getToolAndLine cmd in
    Lwt_process.with_process_full cmd f
  with
  | Unix.Unix_error (err, _, _) ->
    let msg = Unix.error_message err in
    RunAsync.error msg
  | _ ->
    RunAsync.error "error running subprocess"

let clone ?branch ?depth ~dst ~remote () =
  let cmd =
    let open Cmd in
    let cmd = v "git" % "clone" in
    let cmd = match branch with
      | Some branch -> cmd % "--branch" % branch
      | None -> cmd
    in
    let cmd = match depth with
      | Some depth -> cmd % "--depth" % string_of_int depth
      | None -> cmd
    in
    Cmd.(cmd % remote % p dst)
  in
  runGit cmd

let pull ?(force=false) ?depth ~remote ~repo ~branchSpec () =
  let cmd =
    let open Cmd in
    let cmd = v "git" % "-C" % p repo % "pull" in
    let cmd = match force with
      | true -> cmd % "--force"
      | false -> cmd
    in
    let cmd = match depth with
      | Some depth -> cmd % "--depth" % string_of_int depth
      | None -> cmd
    in
    Cmd.(cmd % remote % branchSpec)
  in
  runGit cmd

let checkout ~ref ~repo () =
  let cmd = Cmd.(v "git" % "-C" % p repo % "checkout" % ref) in
  runGit cmd

let lsRemote ?ref ~remote () =
  let open RunAsync.Syntax in
  let ref = Option.orDefault ~default:"master" ref in
  let cmd = Cmd.(v "git"  % "ls-remote" % remote % ref) in
  let%bind out = ChildProcess.runOut cmd in
  match out |> String.trim |> String.split_on_char '\n' with
  | [] ->
    let msg = Printf.sprintf "Unable to resolve ref: %s" ref in
    error msg
  | line::_ ->
    let commit = line |> String.split_on_char '\t' |> List.hd in
    if commit = ""
    then return None
    else return (Some commit)

let isCommitLikeRe = Str.regexp "^[0-9abcdef]+$"
let isCommitLike v =
  let len = String.length v in
  match len with
  | 40 | 6 ->
    Str.string_match isCommitLikeRe v 0
  | _ -> false

module ShallowClone = struct

  let update ~branch ~dst source =
    let open RunAsync.Syntax in
    if%bind Fs.exists dst then

      let%bind remoteCommit = lsRemote ~ref:branch ~remote:source ()
      and localCommit = lsRemote ~ref:branch ~remote:(Path.toString dst) () in

      if remoteCommit = localCommit
      then return ()
      else
        let branchSpec = branch ^ ":" ^ branch in
        pull
          ~branchSpec
          ~depth:1
          ~force:true
          ~remote:source
          ~repo:dst
          ()
    else
      let%bind () = Fs.createDir (Path.parent dst) in
      clone ~branch ~depth:1 ~remote:source ~dst ()
end
