include CommandExprTypes

module V = Value
module E = Expr

let parse src =
  let tokensStore = ref None in
  let getToken lexbuf =
    let tokens =
      match !tokensStore with
      | Some tokens -> tokens
      | None -> CommandExprLexer.read [] lexbuf
    in
    match tokens with
    | tok::rest ->
      tokensStore := Some rest;
      tok
    | [] -> CommandExprParser.EOF
  in
  let lexbuf = Lexing.from_string src in
  let open Run.Syntax in
  try
    return (CommandExprParser.start getToken lexbuf)
  with
  | Failure v ->
    Run.error v
  | CommandExprParser.Error ->
    error "Syntax error"
  | CommandExprLexer.Error (pos, msg) ->
    let cnum = pos.Lexing.pos_cnum - 1 in
    let msg = ParseUtil.formatParseError ~src ~cnum msg in
    error msg

let formatName = function
  | Some namespace, name -> namespace ^ "." ^ name
  | None, name -> name

let eval ~pathSep ~colon ~scope string =
  let open Run.Syntax in
  let%bind expr = parse string in

  let lookupValue name =
    match scope name with
    | Some value -> return value
    | None ->
      let name = formatName name in
      let msg = Printf.sprintf "Undefined variable '%s'" name in
      error msg
  in

  let esyPkgName name = "@opam/" ^ name in

  let rec evalToString expr =
    match%bind eval expr with
    | V.String v -> return v
    | V.Bool true -> return "true"
    | V.Bool false -> return "false"

  and evalToBool expr =
    match%bind eval expr with
    | V.Bool v -> return v
    | V.String _ -> error "Expected bool but got string"

  and eval = function

    (* First rewrite OpamVar syntax into Esy Syntax *)

    (* name *)
    | E.OpamVar ([], var) -> lookupValue (None, "opam:" ^ var)
    (* pkg1+pkg2:enable *)
    | E.OpamVar (pkgNames, "enable") ->
      let cond =
        pkgNames
        |> List.map ~f:(fun name -> E.Var (Some (esyPkgName name), "installed"))
        |> List.fold_left
          ~f:(fun c v -> E.And (c, v))
          ~init:(E.Bool true)
      in
      let expr = E.Condition (cond, E.String "enable", E.String "disable") in
      eval expr
    (* pkg:var *)
    | E.OpamVar ([pkgName], varName) ->
      let expr = E.Var (Some (esyPkgName pkgName), varName) in
      eval expr
    (* pkg1+pkg2:var *)
    | E.OpamVar (pkgNames, varName) ->
      let expr =
        pkgNames
        |> List.map ~f:(fun name -> E.Var (Some (esyPkgName name), varName))
        |> List.fold_left
          ~f:(fun c v -> E.And (c, v))
          ~init:(E.Bool true)
      in
      eval expr

    | E.String s -> return (V.String s)
    | E.Bool b -> return (V.Bool b)
    | E.PathSep -> return (V.String pathSep)
    | E.Colon -> return (V.String colon)
    | E.EnvVar name -> return (V.String ("$" ^ name))
    | E.Var name -> lookupValue name
    | E.Condition (cond, t, e) ->
      if%bind evalToBool cond
      then eval t
      else eval e
    | E.And (a, b) ->
      let%bind a = evalToBool a in
      let%bind b = evalToBool b in
      return (V.Bool (a && b))
    | E.Or (a, b) ->
      let%bind a = evalToBool a in
      let%bind b = evalToBool b in
      return (V.Bool (a || b))
    | E.Not a ->
      let%bind a = evalToBool a in
      return (V.Bool (not a))
    | E.Rel (relop, a, b) ->
      let%bind a = eval a in
      let%bind b = eval b in
      let r =
        match relop with
        | E.EQ -> V.equal a b
        | E.NEQ -> not (V.equal a b)
      in
      return (V.Bool r)
    | E.Concat exprs ->
      let f s expr =
        let%bind v = evalToString expr in
        return (s ^ v)
      in
      let%bind v = Run.List.foldLeft ~f ~init:"" exprs in
      return (V.String v)
  in
  eval expr

let render ?(pathSep="/") ?(colon=":") ~(scope : scope) (string : string) =
  let open Run.Syntax in
  match%bind eval ~pathSep ~colon ~scope string with
  | V.String v -> return v
  | V.Bool true -> return "true"
  | V.Bool false -> return "false"

let%test_module "CommandExpr" = (module struct

  let expectParseOk s expr =
    match parse s with
    | Ok res ->
      if not (E.equal res expr) then (
        Format.printf " Parsing:@[<v 2>@\n%s@]@\n" s;
        Format.printf "Expected:@[<v 2>@\n%a@]@\n" E.pp expr;
        Format.printf "     Got:@[<v 2>@\n%a@]@\n" E.pp res;
        false
      ) else
        true
    | Error err ->
      let msg = Printf.sprintf "parse error: %s" (EsyLib.Run.formatError err) in
      print_endline msg;
      false

  let%test "parse just string" =
    expectParseOk "something" (String "something")

  let%test "parse just string w/ leading space" =
    expectParseOk " something" (String " something")

  let%test "parse just string w/ trailing space" =
    expectParseOk "something " (String "something ")

  let%test "string with squote" =
    expectParseOk "somet'ok'hing" (String "somet'ok'hing") &&
    expectParseOk "somet'ok' hing" (String "somet'ok' hing") &&
    expectParseOk "somet 'ok'hing" (String "somet 'ok'hing")

  let%test "string with dquote" =
    expectParseOk "somet\"ok\"hing" (String "somet\"ok\"hing") &&
    expectParseOk "somet\"ok\" hing" (String "somet\"ok\" hing") &&
    expectParseOk "somet \"ok\"hing" (String "somet \"ok\"hing")

  let%test "parse simple var" =
    expectParseOk "#{hi}" (Var (None, "hi")) &&
    expectParseOk "#{hi }" (Var (None, "hi")) &&
    expectParseOk "#{ hi}" (Var (None, "hi"))

  let%test "parse var+" =
    expectParseOk "#{hi}#{world}" (Concat [
      Var (None, "hi");
      Var (None, "world")
      ])

  let%test "parse string + var" =
    expectParseOk "hello #{world}" (Concat [String "hello "; Var (None, "world")]) &&
    expectParseOk " #{world}" (Concat [String " "; Var (None, "world")]) &&
    expectParseOk "#{world} " (Concat [Var (None, "world"); String " "]) &&
    expectParseOk "hello#{world}" (Concat [String "hello"; Var (None, "world")]) &&
    expectParseOk "#{hello} world" (Concat [Var (None, "hello"); String " world"]) &&
    expectParseOk "#{hello}world" (Concat [Var (None, "hello"); String "world"])

  let%test "parse complex var" =
    expectParseOk "#{hi world}" (Concat [Var (None, "hi"); Var (None, "world")])
    && expectParseOk "#{h-i world}" (Concat [Var (None, "h-i"); Var (None, "world")])
    && expectParseOk "#{hi :}" (Concat [Var (None, "hi"); Colon])
    && expectParseOk "#{hi : world}" (Concat [Var (None, "hi"); Colon; Var (None, "world")])
    && expectParseOk "#{hi /}" (Concat [Var (None, "hi"); PathSep])
    && expectParseOk "#{hi / world}" (Concat [Var (None, "hi"); PathSep; Var (None, "world")])

  let%test "parse var with env vars" =
    expectParseOk "#{hi / $world}" (Concat [Var (None, "hi"); PathSep; EnvVar "world"])

  let%test "parse var with literals" =
    expectParseOk "#{'world'}" (String "world")
    && expectParseOk "#{/ 'world'}" (Concat [PathSep; String "world"])
    && expectParseOk "#{: 'world'}" (Concat [Colon; String "world"])
    && expectParseOk "#{/'world'}" (Concat [PathSep; String "world"])
    && expectParseOk "#{:'world'}" (Concat [Colon; String "world"])
    && expectParseOk "#{'world' /}" (Concat [String "world"; PathSep])
    && expectParseOk "#{'world' :}" (Concat [String "world"; Colon])
    && expectParseOk "#{'world'/}" (Concat [String "world"; PathSep])
    && expectParseOk "#{'world':}" (Concat [String "world"; Colon])
    && expectParseOk "#{hi'world'}" (Concat [Var (None, "hi"); String "world"])
    && expectParseOk "#{'world'hi}" (Concat [String "world"; Var (None, "hi")])
    && expectParseOk "#{hi / 'world'}" (Concat [Var (None, "hi"); PathSep; String "world"])
    && expectParseOk "#{'hi''world'}" (Concat [String "hi";  String "world"])
    && expectParseOk "#{'h\\'i'}" (String "h'i")

  let%test "parse namespace" =
    expectParseOk "#{ns.hi}" (Var (Some "ns", "hi"))
    && expectParseOk "#{n-s.hi}" (Var (Some "n-s", "hi"))
    && expectParseOk "#{@scope/pkg.hi}" (Var (Some "@scope/pkg", "hi"))
    && expectParseOk "#{@s-cope/pkg.hi}" (Var (Some "@s-cope/pkg", "hi"))
    && expectParseOk "#{@scope/pkg.hi 'hey'}" (Concat [
      Var (Some "@scope/pkg", "hi");
      String ("hey");
    ])

  let%test "parse conditionals (strings in then / else)" =
    expectParseOk
      "#{lwt.installed ? '--enable-lwt' : '--disable-lwt'}"
      (Condition (
        (Var (Some "lwt", "installed")),
        (String "--enable-lwt"),
        (String "--disable-lwt")
        ))

  let%test "parse conditionals (vars in then / else)" =
    expectParseOk
      "#{lwt.installed ? then : else}"
      (Condition (
        (Var (Some "lwt", "installed")),
        (Var (None, "then")),
        (Var (None, "else"))
        ))

  let%test "parse conditionals (lists in then / else)" =
    expectParseOk
      "#{lwt.installed ? (then : then) : (else : else)}"
      (Condition (
        (Var (Some "lwt", "installed")),
        (Concat [Var (None, "then"); Colon; Var (None, "then")]),
        (Concat [Var (None, "else"); Colon; Var (None, "else")])
        ))

  let%test "parse conj" =
    expectParseOk
      "#{lwt.installed && async.installed}"
      (And (
        (Var (Some "lwt", "installed")),
        (Var (Some "async", "installed"))
        ))

  let%test "parse disj" =
    expectParseOk
      "#{lwt.installed || async.installed}"
      (Or (
        (Var (Some "lwt", "installed")),
        (Var (Some "async", "installed"))
        ))

  let%test "parse eq" =
    expectParseOk
      "#{lwt.installed == async.installed}"
      (Rel (
        EQ,
        (Var (Some "lwt", "installed")),
        (Var (Some "async", "installed"))
        ))

  let%test "parse neq" =
    expectParseOk
      "#{lwt.installed != async.installed}"
      (Rel (
        NEQ,
        (Var (Some "lwt", "installed")),
        (Var (Some "async", "installed"))
        ))

  let%test "parse precedence disj / conj" =
    expectParseOk
      "#{lwt || async && mirage}"
      (Or (
        Var (None, "lwt"),
        And (
          Var (None, "async"),
          Var (None, "mirage")
        )
      ))
    && expectParseOk
      "#{mirage && lwt || async}"
      (Or (
        And (
          Var (None, "mirage"),
          Var (None, "lwt")
        ),
        Var (None, "async")
      ))
    && expectParseOk
      "#{(lwt || async) && mirage}"
      (And (
        Or (
          Var (None, "lwt"),
          Var (None, "async")
        ),
        Var (None, "mirage")
      ))
    && expectParseOk
      "#{mirage && (lwt || async)}"
      (And (
        Var (None, "mirage"),
        Or (
          Var (None, "lwt"),
          Var (None, "async")
        )
      ))

  let%test "parse precedence conj / eq" =
    expectParseOk
      "#{lwt == async && mirage}"
      (And (
        Rel (
          EQ,
          Var (None, "lwt"),
          Var (None, "async")
        ),
        Var (None, "mirage")
      ))
    && expectParseOk
      "#{lwt != async && mirage}"
      (And (
        Rel (
          NEQ,
          Var (None, "lwt"),
          Var (None, "async")
        ),
        Var (None, "mirage")
      ))
    && expectParseOk
      "#{mirage && lwt == async}"
      (And (
        Var (None, "mirage"),
        Rel (
          EQ,
          Var (None, "lwt"),
          Var (None, "async")
        )
      ))
    && expectParseOk
      "#{mirage && lwt != async}"
      (And (
        Var (None, "mirage"),
        Rel (
          NEQ,
          Var (None, "lwt"),
          Var (None, "async")
        )
      ))
    && expectParseOk
      "#{(mirage && lwt) != async}"
        (Rel (
          NEQ,
          And (
            Var (None, "mirage"),
            Var (None, "lwt")
          ),
          Var (None, "async")
        ))

  let%test "parse precedence not" =
    expectParseOk
      "#{!lwt == async}"
      (Rel (
        EQ,
        Not (Var (None, "lwt")),
        Var (None, "async")
      ))
    && expectParseOk
      "#{lwt == !async}"
      (Rel (
        EQ,
        Var (None, "lwt"),
        Not (Var (None, "async"))
      ))
    && expectParseOk
      "#{!lwt != async}"
      (Rel (
        NEQ,
        Not (Var (None, "lwt")),
        Var (None, "async")
      ))
    && expectParseOk
      "#{lwt != !async}"
      (Rel (
        NEQ,
        Var (None, "lwt"),
        Not (Var (None, "async"))
      ))

  let%test "parse opam global" =
    expectParseOk
      "%{name}%"
      (OpamVar ([], "name"))
    && expectParseOk
      "%{name }%"
      (OpamVar ([], "name"))
    && expectParseOk
      "%{ name}%"
      (OpamVar ([], "name"))

  let%test "parse opam with surroundings" =
    expectParseOk
      "%{name}%!"
      (Concat [OpamVar ([], "name"); String "!"])
    && expectParseOk
      "hey, %{name}%!"
      (Concat [String "hey, "; OpamVar ([], "name"); String "!"])

  let%test "parse opam scoped" =
    expectParseOk
      "%{name:var}%"
      (OpamVar (["name"], "var"))
    && expectParseOk
      "%{name :var}%"
      (OpamVar (["name"], "var"))
    && expectParseOk
      "%{name: var}%"
      (OpamVar (["name"], "var"))

  let%test "parse opam scoped with +" =
    expectParseOk
      "%{pkg1+pkg2:var}%"
      (OpamVar (["pkg1"; "pkg2"], "var"))
    && expectParseOk
      "%{pkg1 +pkg2:var}%"
      (OpamVar (["pkg1"; "pkg2"], "var"))
    && expectParseOk
      "%{pkg1+ pkg2:var}%"
      (OpamVar (["pkg1"; "pkg2"], "var"))

  let expectRenderOk scope s expected =
    match render ~scope s with
    | Ok v ->
      if v <> expected then (
        Format.printf "  Render:@[<v 2>@\n%s@]@\n" s;
        Format.printf "Expected:@[<v 2>@\n%s@]@\n" expected;
        Format.printf "     Got:@[<v 2>@\n%s@]@\n" v;
        false
      ) else
        true
    | Error err ->
      let msg = Printf.sprintf "error: %s" (EsyLib.Run.formatError err) in
      print_endline msg;
      false

  let expectRenderError scope s expectedError =
    match render ~scope s with
    | Ok _ -> false
    | Error error ->
      let error = error |> EsyLib.Run.formatError |> String.trim in
      if (String.trim expectedError) <> error then (
        Printf.printf "Expected: %s\n" expectedError;
        Printf.printf "     Got: %s\n" error;
        false
      ) else true

  let scope = function
  | None, "name" -> Some (Value.String "pkg")
  | None, "isTrue" -> Some (Value.Bool true)
  | None, "isFalse" -> Some (Value.Bool false)
  | None, "opam:os" -> Some (Value.String "MSDOS")
  | None, "os" -> Some (Value.String "OS/2")
  | Some "self", "lib" -> Some (Value.String "store/lib")
  | Some "@opam/pkg", "lib" -> Some (Value.String "store/opam-pkg/lib")
  | Some "@opam/pkg1", "installed" -> Some (Value.Bool true)
  | Some "@opam/pkg2", "installed" -> Some (Value.Bool true)
  | Some "@opam/pkg-not", "installed" -> Some (Value.Bool false)
  | _ -> None

  let%test "render" =

    expectRenderOk scope "Hello, #{name}!" "Hello, pkg!"
    && expectRenderOk scope "#{self.lib / $NAME}" "store/lib/$NAME"
    && expectRenderOk scope "#{isTrue ? 'ok' : 'oops'}" "ok"
    && expectRenderOk scope "#{isFalse ? 'oops' : 'ok'}" "ok"
    && expectRenderOk scope "#{!isFalse ? 'ok' : 'oops'}" "ok"
    && expectRenderOk scope "#{isFalse && isTrue ? 'oops' : 'ok'}" "ok"
    && expectRenderOk scope "#{isFalse || isTrue ? 'ok' : 'oops'}" "ok"
    && expectRenderOk scope "#{os == 'OS/2' ? 'ok' : 'oops'}" "ok"
    && expectRenderOk scope "#{os != 'macOs' ? 'ok' : 'oops'}" "ok"


  let%test "render opam" =
    expectRenderOk scope "Hello, %{os}%!" "Hello, MSDOS!"
    && expectRenderOk scope "%{pkg:lib}%" "store/opam-pkg/lib"
    && expectRenderOk scope "%{pkg1:enable}%" "enable"
    && expectRenderOk scope "%{pkg-not:enable}%" "disable"
    && expectRenderOk scope "%{pkg1+pkg2:enable}%" "enable"
    && expectRenderOk scope "%{pkg1+pkg-not:enable}%" "disable"
    && expectRenderOk scope "%{pkg1:installed}%" "true"
    && expectRenderOk scope "%{pkg-not:installed}%" "false"
    && expectRenderOk scope "%{pkg1+pkg2:installed}%" "true"
    && expectRenderOk scope "%{pkg1+pkg-not:installed}%" "false"

  let%test "render errors" =
    expectRenderError scope "#{unknown}" "Error: Undefined variable 'unknown'"
    && expectRenderError scope "#{ns.unknown}" "Error: Undefined variable 'ns.unknown'"
    && expectRenderError scope "#{ns.unknown" "Error: unexpected end of string:
  >
  > #{ns.unknown...
  >            ^"
    && expectRenderError scope "#{'some" "Error: unexpected end of string:
  >
  > #{'some...
  >       ^"
    && expectRenderError scope "#{'some}" "Error: unexpected end of string:
  >
  > #{'some}...
  >        ^"
    && expectRenderError scope "#{cond ^}" "Error: unexpected token '^' found:
  >
  > #{cond ^}...
  >        ^"

  let%test "syntax errors" =
    expectRenderError scope "#{cond &&}" "Error: Syntax error"
    && expectRenderError scope "#{cond ?}" "Error: Syntax error"
    && expectRenderError scope "#{cond ? then}" "Error: Syntax error"
    && expectRenderError scope "#{cond ? then :}" "Error: Syntax error"

end)
