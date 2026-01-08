(** Unroll the let+ if the tuple contains a function pointer. We don't want to
    unfold be a function is called on each element. *)

module Env = struct
  type t = bool Ident.TermIdent.Map.t

  let add variable b env = Ident.TermIdent.Map.add variable b env
  let find variable env = Ident.TermIdent.Map.find variable env
  let arity _ _env : int = failwith ""
end

let rec f env term bindings constrs =
  match constrs with
  | [] ->
      List.fold_right
        (fun (variable, sterm) term ->
          Term.Let { variable; term = sterm; k = term })
        bindings term
  | constr :: constrs ->
      let arity = Env.arity constr env in
      let terms =
        List.init arity @@ fun index ->
        let bindings =
          List.map
            (fun (variable, lterm) -> (variable, Term.Lookup { lterm; index }))
            bindings
        in
        f env term bindings constrs
      in
      Term.Constructor { ty = constr; terms }

let unroll variable lterm ands prefix term =
  let variable' = Ident.TermIdent.refresh "unfold" variable in
  let ands' =
    List.map
      (fun (variable, _) -> Ident.TermIdent.refresh "unfold" variable)
      ands
  in
  let () = ignore (lterm, prefix, term, variable', ands') in
  failwith ""

let rec cterm_unroll env = function
  | Term.LetPlus { variable; lterm; ands; prefix; term } -> (
      (* TODO: we need the type of `term` *)
      let lis, lterm = sterm_unroll env lterm in
      let iss, ands =
        ands
        |> List.map (fun (variable, lterm) ->
            let is, lterm = sterm_unroll env lterm in
            (is, (variable, lterm)))
        |> List.split
      in
      let unroll = List.exists Fun.id (lis :: iss) in
      let () = ignore (variable, prefix, term, lterm, ands) in
      match unroll with true -> failwith "" | false -> failwith "")
  | Term.(True | False) as e -> (false, e)
  | Constructor { ty; terms } ->
      let contains, terms =
        terms |> List.map (cterm_unroll env) |> List.split
      in
      let contains = List.exists Fun.id contains in
      (contains, Constructor { ty; terms })
  | Let { variable; term; k } ->
      let fn, term = sterm_unroll env term in
      let env = Env.add variable fn env in
      let fn, k = cterm_unroll env k in
      (fn, Let { variable; term; k })
  | Log { message; variables; k } ->
      let is, k = cterm_unroll env k in
      (is, Log { message; variables; k })
  | Synth sterm ->
      let fn, sterm = sterm_unroll env sterm in
      (fn, Synth sterm)

and sterm_unroll env = function
  | Term.Var variable as v ->
      let is = Env.find variable env in
      (is, v)
  | Fn _ as e -> (true, e)
  | Lookup { lterm; index } ->
      let is, lterm = sterm_unroll env lterm in
      (is, Lookup { lterm; index })
  | Reindex { lhs; rhs; lterm } ->
      let is, lterm = sterm_unroll env lterm in
      (is, Reindex { lhs; rhs; lterm })
  | Circ lterm ->
      let is, lterm = sterm_unroll env lterm in
      (is, Circ lterm)
  | Lift { tys; func } ->
      (* is should be true *)
      let is, func = sterm_unroll env func in
      (is, Lift { tys; func })
  | FnCall _ ->
      (* TODO: how to do without type info ? *)
      failwith ""
  | Operator operator ->
      let f (is, env) cterm =
        let i, te = cterm_unroll env cterm in
        ((i || is, env), te)
      in
      let (is, _), operator = Operator.traverse f (false, env) operator in
      (is, Operator operator)
  | Ann (cterm, ty) ->
      let is, cterm = cterm_unroll env cterm in
      (is, Ann (cterm, ty))

let function_unroll _env function_decl =
  let Prog.{ fn_name; signature; ops; args; body } = function_decl in
  let () = ignore (fn_name, signature, ops, args, body) in
  failwith ""
