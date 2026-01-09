(*
  Create a specialized version of function where all function's calls involving
  a pointer function are replaced with a conterpart with the pointer pointer
  inlined.
  
  
  Assume [Unfold], otherwise function pointers info inside [Constructor] are 
  lost when doing a let+. 

*)

module Env = struct
  module Fns = Map.Make (struct
    type t = Ident.FnIdent.t * Ident.FnIdent.t list

    let compare = Stdlib.compare
  end)

  type t = {
    variables : Ident.scoped Term.sterm_ Ident.TermIdent.Map.t;
    functions : Prog.fndecl Fns.t;
  }

  let add variable sterm env =
    {
      env with
      variables = Ident.TermIdent.Map.add variable sterm env.variables;
    }

  let find_variable ident env = Ident.TermIdent.Map.find ident env.variables

  let rec find_constrs_sterm env = function
    | Term.Var v -> find_constrs_sterm env (find_variable v env)
    | Lookup { lterm; index } ->
        let _, lterm = find_constrs_sterm env lterm in
        find_constrs_cterm env (List.nth lterm index)
    | Reindex { lhs = _; rhs = _; lterm } -> find_constrs_sterm env lterm
    | Circ _ -> failwith "TODO: since circ is computed."
    | FnCall _ -> failwith ""
    | Ann (cterm, _) -> find_constrs_cterm env cterm
    | Fn _ | Lift _ | Operator _ -> assert false

  and find_constrs_cterm env = function
    | Term.(False | True) -> assert false
    | Constructor { ty; terms } -> (ty, terms)
    | Let { variable = _; term = _; k } -> find_constrs_cterm env k
    | LetPlus _ -> failwith ""
    | Log { message = _; variables = _; k = _ } -> failwith ""
    | Synth sterm -> find_constrs_sterm env sterm

  (* 
    Return env since we will create the function node 
    if we enconder a [Lift]. 
  *)
  let rec find_fn_ident_sterm env = function
    | Term.Var s -> find_fn_ident_sterm env (find_variable s env)
    | Fn { fn_ident } -> (env, fn_ident)
    | Term.Lift _ -> failwith ""
    | Lookup { lterm; index } ->
        let _, terms = find_constrs_sterm env lterm in
        find_fn_ident_cterm env (List.nth terms index)
    | Reindex { lterm; _ } -> find_fn_ident_sterm env lterm
    | Circ _ -> failwith "TODO: since circ is computed."
    | FnCall _ -> failwith ""
    | Operator _ -> failwith ""
    | Ann (cterm, _) -> find_fn_ident_cterm env cterm

  and find_fn_ident_cterm _env = function _ -> failwith ""
end

let rec cterm_mono env = function
  | Term.(True | False) as e -> (env, e)
  | Constructor { ty; terms } ->
      let env, terms = List.fold_left_map cterm_mono env terms in
      (env, Constructor { ty; terms })
  | Let { variable; term; k } ->
      let env, term = sterm_mono env term in
      let env, k = cterm_mono env k in
      (env, Let { variable; term; k })
  | LetPlus { variable; lterm; ands; prefix; term } ->
      let env, lterm = sterm_mono env lterm in
      let env, ands =
        List.fold_left_map
          (fun env (variable, sterm) ->
            let env, sterm = sterm_mono env sterm in
            (env, (variable, sterm)))
          env ands
      in
      let env, term = cterm_mono env term in
      (env, LetPlus { variable; lterm; ands; prefix; term })
  | Log { message; variables; k } ->
      let env, k = cterm_mono env k in
      (env, Log { message; variables; k })
  | Synth sterm ->
      let env, sterm = sterm_mono env sterm in
      (env, Synth sterm)

and sterm_mono env = function
  | Term.FnCall { fn; ty_resolve; dicts; args } ->
      let env, args = List.fold_left_map cterm_mono env args in
      let () = ignore (fn, ty_resolve, dicts, env, args) in
      failwith ""
  | Term.Lift _ -> failwith ""
  | (Term.Var _ | Term.Fn _) as e -> (env, e)
  | Lookup { lterm; index } ->
      let env, lterm = sterm_mono env lterm in
      (env, Lookup { lterm; index })
  | Reindex { lhs; rhs; lterm } ->
      let env, lterm = sterm_mono env lterm in
      (env, Reindex { lhs; rhs; lterm })
  | Circ lterm ->
      let env, lterm = sterm_mono env lterm in
      (env, Circ lterm)
  | Operator operator ->
      let env, operator = Operator.traverse cterm_mono env operator in
      (env, Operator operator)
  | Ann (cterm, ty) ->
      let env, cterm = cterm_mono env cterm in
      (env, Ann (cterm, ty))

let monomorph _env fun_decl =
  let Prog.{ fn_name; signature; ops; args; body } = fun_decl in
  let () = ignore (fn_name, signature, ops, args, body) in
  failwith ""
