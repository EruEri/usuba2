let mono_cterm env = function
  | Term.True -> (env, (None, Term.True))
  | False -> (env, (None, False))
  | Constructor { ty; terms } ->
      let () = ignore (ty, terms) in
      failwith ""
  | Let { variable; term; k } ->
      let () = ignore (variable, term, k) in
      failwith ""
  | LetPlus { variable; lterm; ands; prefix; term } ->
      let () = ignore (variable, lterm, ands, prefix, term) in
      failwith ""
  | Log { message; variables; k } ->
      let () = ignore (message, variables, k) in
      failwith ""
  | Synth _ -> failwith ""

and mon_sterm _env = function
  | Term.Var _ -> failwith ""
  | Fn _ -> failwith ""
  | Lookup _ -> failwith ""
  | Reindex _ -> failwith ""
  | Circ _ -> failwith ""
  | Lift _ -> failwith ""
  | FnCall _ -> failwith ""
  | Operator _ -> failwith ""
  | Ann _ -> failwith ""

let monomorph _env fun_decl =
  let Prog.{ fn_name; signature; ops; args; body } = fun_decl in
  let () = ignore (fn_name, signature, ops, args, body) in
  failwith ""
