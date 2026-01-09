(** unfold the let+ if the tuple contains a function pointer. We don't want to
    unfold be a function is called on each element. *)

module Env = struct
  type t = bool Ident.TermIdent.Map.t

  let add variable b env = Ident.TermIdent.Map.add variable b env
  let find variable env = Ident.TermIdent.Map.find variable env
  let arity _ _env : int = failwith ""
end

let rec unfold' env term bindings constrs =
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
        unfold' env term bindings constrs
      in
      Term.Constructor { ty = constr; terms }

let unfold env variable lterm ands prefix term =
  let variable' = Ident.TermIdent.refresh "unfold" variable in
  let ands' =
    List.map
      (fun (variable, _) -> Ident.TermIdent.refresh "unfold" variable)
      ands
  in
  let bindings = (variable, lterm) :: ands in
  let variables_fresh = variable' :: ands' in
  let variables =
    List.map2
      (fun (variable, _) variable' -> (variable, Term.Var variable'))
      bindings variables_fresh
  in
  let body = unfold' env term variables prefix in
  List.fold_right2
    (fun (_variable, lterm) variable' body ->
      Term.Let { variable = variable'; term = lterm; k = body })
    bindings variables_fresh body

let rec cterm_unfold env = function
  | Term.LetPlus { variable; lterm; ands; prefix; term } -> (
      (* TODO: we need the type of `term` *)
      let lis, lterm = sterm_unfold env lterm in
      let iss, ands =
        ands
        |> List.map (fun (variable, lterm) ->
            let is, lterm = sterm_unfold env lterm in
            (is, (variable, lterm)))
        |> List.split
      in
      let unfold = List.exists Fun.id (lis :: iss) in
      let () = ignore (variable, prefix, term, lterm, ands) in
      match unfold with true -> failwith "" | false -> failwith "")
  | Term.(True | False) as e -> (false, e)
  | Constructor { ty; terms } ->
      let contains, terms =
        terms |> List.map (cterm_unfold env) |> List.split
      in
      let contains = List.exists Fun.id contains in
      (contains, Constructor { ty; terms })
  | Let { variable; term; k } ->
      let fn, term = sterm_unfold env term in
      let env = Env.add variable fn env in
      let fn, k = cterm_unfold env k in
      (fn, Let { variable; term; k })
  | Log { message; variables; k } ->
      let is, k = cterm_unfold env k in
      (is, Log { message; variables; k })
  | Synth sterm ->
      let fn, sterm = sterm_unfold env sterm in
      (fn, Synth sterm)

and sterm_unfold env = function
  | Term.Var variable as v ->
      let is = Env.find variable env in
      (is, v)
  | Fn _ as e -> (true, e)
  | Lookup { lterm; index } ->
      let is, lterm = sterm_unfold env lterm in
      (is, Lookup { lterm; index })
  | Reindex { lhs; rhs; lterm } ->
      let is, lterm = sterm_unfold env lterm in
      (is, Reindex { lhs; rhs; lterm })
  | Circ lterm ->
      let is, lterm = sterm_unfold env lterm in
      (is, Circ lterm)
  | Lift { tys; func } ->
      (* is should be true *)
      let is, func = sterm_unfold env func in
      (is, Lift { tys; func })
  | FnCall _ ->
      (* TODO: how to do without type info ? *)
      failwith ""
  | Operator operator ->
      let f (is, env) cterm =
        let i, te = cterm_unfold env cterm in
        ((i || is, env), te)
      in
      let (is, _), operator = Operator.traverse f (false, env) operator in
      (is, Operator operator)
  | Ann (cterm, ty) ->
      let is, cterm = cterm_unfold env cterm in
      (is, Ann (cterm, ty))

let function_unfold env (function_decl : Prog.fndecl) =
  let _, body = cterm_unfold env function_decl.body in
  { function_decl with body }
