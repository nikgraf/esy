(**
 * Work with remote URLs via curl utility.
 *)

type response =
  | Success of string
  | NotFound

type url = string

val getOrNotFound :
  ?accept : string
  -> url
  -> response RunAsync.t

val get :
  ?accept : string
  -> url
  -> string RunAsync.t

val download :
  output:Fpath.t
  -> url
  -> unit RunAsync.t
