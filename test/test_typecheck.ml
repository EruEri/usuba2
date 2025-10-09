open Alcotest

let x = Ua0.Ast.TermIdent.fresh "x"
let y = Ua0.Ast.TermIdent.fresh "y"
let z = Ua0.Ast.TermIdent.fresh "z"
let env0 = failwith "NYI"
let typs = failwith "NYI"

let () =
  run "typesynth"
    [
      ( "var",
        [
          test_case "var-in" `Quick (fun () ->
              check typs "`x` has type `bool` in `env0`"
                (Uat.Typecheck.typesynth env0 (Var x))
                Ua0.Ast.TyBool);
          test_case "var-in" `Quick (fun () ->
              check typs "`x` has type `bool` in `env0`"
                (Uat.Typecheck.typesynth env0 (Var x))
                Ua0.Ast.TyBool);
        ] );
    ]
