module F = SemverVersion.Formula;
module C = SemverVersion.Formula.Constraint;

let singleDnf = c => F.OR([F.AND([c])]);

let fromPrefix = (op, version) : F.DNF.t => {
  let v = SemverVersion.Version.parseExn(version);
  switch (op) {
  | `Eq => singleDnf(C.EQ(v))
  | `Geq => singleDnf(C.GTE(v))
  | `Leq => singleDnf(C.LTE(v))
  | `Lt => singleDnf(C.LT(v))
  | `Gt => singleDnf(C.GT(v))
  | `Neq => F.OR([F.AND([C.GT(v)]), F.AND([C.LT(v)])])
  };
};

let rec getOCamlVersion = opamvalue : F.DNF.t =>
  /* Shared.VersionFormula.ANY */
  OpamParserTypes.(
    switch (opamvalue) {
    | Logop(_, `And, left, right) =>
      F.DNF.conj(getOCamlVersion(left), getOCamlVersion(right))
    | Logop(_, `Or, left, right) =>
      F.DNF.disj(getOCamlVersion(left), getOCamlVersion(right))
    | Relop(_, _rel, Ident(_, "ocaml-version"), String(_, "broken")) => F.any
    | Relop(_, rel, Ident(_, "ocaml-version"), String(_, version)) =>
      fromPrefix(rel, version)
    /* We don't support pre-4.02.3 anyway */
    | Relop(_, `Neq, Ident(_, "compiler"), String(_, "4.02.1+BER")) => F.any
    | Relop(_, `Eq, Ident(_, "compiler"), String(_, _)) => F.any
    | Relop(_, _rel, Ident(_, "opam-version"), _) => F.any /* TODO should I care about this? */
    | Relop(_, _rel, Ident(_, "os"), String(_, _version)) => F.any
    | Pfxop(_, `Not, Ident(_, "preinstalled")) => F.any
    | Ident(_, "preinstalled" | "false") => F.any
    | Bool(_, true) => F.any
    | Bool(_, false) => singleDnf(C.NONE)
    | Option(_, contents, options) =>
      print_endline(
        "Ignoring option: "
        ++ (
          options |> List.map(~f=OpamPrinter.value) |> String.concat(" .. ")
        ),
      );
      getOCamlVersion(contents);
    | List(_, items) =>
      let rec loop = items =>
        switch (items) {
        | [] => F.any
        | [item] => getOCamlVersion(item)
        | [item, ...rest] => F.DNF.conj(getOCamlVersion(item), loop(rest))
        };
      loop(items);
    | Group(_, items) =>
      let rec loop = items =>
        switch (items) {
        | [] => F.any
        | [item] => getOCamlVersion(item)
        | [item, ...rest] => F.DNF.conj(getOCamlVersion(item), loop(rest))
        };
      loop(items);
    | _y =>
      print_endline(
        "Unexpected option -- pretending its any "
        ++ OpamPrinter.value(opamvalue),
      );
      F.any;
    }
  );

let rec getAvailability = opamvalue =>
  /* Shared.VersionFormula.ANY */
  OpamParserTypes.(
    switch (opamvalue) {
    | Logop(_, `And, left, right) =>
      getAvailability(left) && getAvailability(right)
    | Logop(_, `Or, left, right) =>
      getAvailability(left) || getAvailability(right)
    | Relop(_, _rel, Ident(_, "ocaml-version"), String(_, "broken")) =>
      false
    | Relop(_, _rel, Ident(_, "ocaml-version"), String(_, _version)) => true
    /* We don't support pre-4.02.3 anyway */
    | Relop(_, `Neq, Ident(_, "compiler"), String(_, "4.02.1+BER")) => true
    | Relop(_, `Eq, Ident(_, "compiler"), String(_, _compiler)) =>
      /* print_endline("Wants a compiler " ++ compiler ++ "... assuming we don't have it"); */
      false
    | Relop(_, _rel, Ident(_, "opam-version"), _) => true
    | Relop(_, `Eq, Ident(_, "os"), String(_, "darwin")) => true
    | Relop(_, `Neq, Ident(_, "os"), String(_, "darwin")) => false
    | Relop(_, _rel, Ident(_, "os"), String(_, _os)) => false
    | Pfxop(_, `Not, Ident(_, "preinstalled")) => true
    | Ident(_, "preinstalled") => false
    | Bool(_, false) => false
    | Bool(_, true) => true
    | Option(_, contents, options) =>
      print_endline(
        "[[ AVAILABILITY ]] Ignoring option: "
        ++ (
          options |> List.map(~f=OpamPrinter.value) |> String.concat(" .. ")
        ),
      );
      getAvailability(contents);
    | List(_, items) =>
      let rec loop = items =>
        switch (items) {
        | [] => true
        | [item] => getAvailability(item)
        | [item, ...rest] => getAvailability(item) && loop(rest)
        };
      loop(items);
    | Group(_, items) =>
      let rec loop = items =>
        switch (items) {
        | [] => true
        | [item] => getAvailability(item)
        | [item, ...rest] => getAvailability(item) && loop(rest)
        };
      loop(items);
    | _y =>
      print_endline(
        "Unexpected availability option -- pretending its fine "
        ++ OpamPrinter.value(opamvalue),
      );
      true;
    }
  );
