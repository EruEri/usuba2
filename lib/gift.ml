open Ast

module Ty = struct
  let bool = TyBool
  let eapp name = TyApp { name; ty_args = None }
  let app name ty_args = TyApp { name; ty_args = Some ty_args }
  let tuple size ty = TyTuple { size; ty }

  let fn ty_vars parameters return_type =
    TyFun { ty_vars; parameters; return_type }

  let v name = TyVarApp { name; ty_args = None }
  let varapp name ty_args = TyVarApp { name; ty_args = Some ty_args }
end

module Expression = struct
  let true' = ETrue
  let false' = EFalse

  let e_indexing e slice index =
    EIndexing { expression = e; indexing = { name = slice; index } }

  let indexing s slice index = e_indexing (EVar s) slice index
  let builin_call builtin ty_args args = EBuiltinCall { builtin; ty_args; args }

  let fn_call fn_name ty_args args =
    EFunctionCall { fn_name = Left fn_name; ty_args; args }

  let term_call fn_name ty_args args =
    EFunctionCall { fn_name = Right fn_name; ty_args; args }

  let ( land ) lhs rhs = EOp (BAnd (lhs, rhs))
  let ( lor ) lhs rhs = EOp (BOr (lhs, rhs))
  let ( lxor ) lhs rhs = EOp (BXor (lhs, rhs))

  let ( |> ) e ty_args fn_name =
    EFunctionCall { fn_name = Either.left fn_name; ty_args; args = [ e ] }

  let let_plus variable ty_arg ty_ret expression ands k =
    let variable = TermIdent.fresh variable in
    let ands =
      List.map
        (fun (variable, expression) ->
          let variable = TermIdent.fresh variable in
          (variable, expression))
        ands
    in
    let statements, expression' = k variable (List.map fst ands) in
    SLetPLus
      {
        variable;
        ty_arg;
        ty_ret;
        expression;
        ands;
        body = { statements; expression = expression' };
      }

  let lnot expr = EOp (Unot expr)
  let v s = EVar s
  let fv s = EFunVar (s, None)
  let fv_t s tys = EFunVar (s, Some tys)
  let vars idents = List.map v idents
end

module Statement = struct
  let decl variable expression k =
    let variable = TermIdent.fresh variable in
    let statements, finale = k variable in
    (StDeclaration { variable; expression } :: statements, finale)

  let cstr variable ty expressions k =
    let variable = TermIdent.fresh variable in
    let statements, finale = k variable in
    (StConstructor { variable; ty; expressions } :: statements, finale)

  let log variables k =
    let statements, finale = k () in
    (StLog variables :: statements, finale)
end

let app = FnIdent.fresh "app"
let map2 = FnIdent.fresh "map2"
let map = FnIdent.fresh "map"
let row = TyDeclIdent.fresh "row"
let col = TyDeclIdent.fresh "col"
let keys = TyDeclIdent.fresh "keys"
let slice = TyDeclIdent.fresh "slice"
let state = TyDeclIdent.fresh "state"
let subcells = FnIdent.fresh "subcells"
let add_round_key = FnIdent.fresh "add_round_key"
let transpose = FnIdent.fresh "transpose"
let reindex_cols_row = FnIdent.fresh "reindex_cols_row"
let col_reverse = FnIdent.fresh "col_reverse"
let rev_rotate_0 = FnIdent.fresh "rev_rotate_0"
let rev_rotate_1 = FnIdent.fresh "rev_rotate_1"
let rev_rotate_2 = FnIdent.fresh "rev_rotate_2"
let rev_rotate_3 = FnIdent.fresh "rev_rotate_3"
let permbits = FnIdent.fresh "permbits"
let row_ror_0 = FnIdent.fresh "row_ror_0"
let row_ror_1 = FnIdent.fresh "row_ror_1"
let row_ror_2 = FnIdent.fresh "row_ror_2"
let row_ror_3 = FnIdent.fresh "row_ror_3"
let fxor = FnIdent.fresh "fxor"
let round = FnIdent.fresh "round"
let fngift = FnIdent.fresh "gift"

let gift =
  [
    (let alpha = TyIdent.fresh "'a" in
     KnTypedecl
       {
         ty_vars = [ alpha ];
         ty_name = row;
         definition = TyTuple { size = 4; ty = Ty.v alpha };
       });
    (let alpha = TyIdent.fresh "'a" in
     KnTypedecl
       {
         ty_vars = [ alpha ];
         ty_name = col;
         definition = Ty.(tuple 4 (v alpha));
       });
    (let alpha = TyIdent.fresh "'a" in
     KnTypedecl
       {
         ty_vars = [ alpha ];
         ty_name = slice;
         definition = TyTuple { size = 4; ty = Ty.v alpha };
       });
    KnTypedecl
      {
        ty_vars = [];
        ty_name = state;
        definition = Ty.(app col (app row (app slice bool)));
      };
    KnTypedecl
      { ty_vars = []; ty_name = keys; definition = Ty.(tuple 28 @@ eapp state) };
    (let alpha = TyIdent.fresh "'a" in
     let beta = TyIdent.fresh "'b" in
     let ctrl = TyIdent.fresh "#t" in
     let ty_alpha = Ty.(v alpha) in
     let ty_beta = Ty.(v beta) in
     let ty_fn = Ty.(fn [] [ ty_alpha ] ty_beta) in
     let ty_ctrl_fn = Ty.(varapp ctrl ty_fn) in
     let ty_ctrl_alpha = Ty.(varapp ctrl ty_alpha) in
     let ty_ctrl_beta = Ty.(varapp ctrl ty_beta) in
     let fs = TermIdent.fresh "fs" in
     let xs = TermIdent.fresh "xs" in
     let expression =
       Expression.let_plus "f"
         Ty.(v ctrl)
         ty_beta
         Expression.(v fs)
         Expression.[ ("x", v xs) ]
       @@ fun f ands ->
       let x = match ands with [] -> assert false | x :: _ -> x in
       ([], Expression.(term_call f [] [ v x ]))
     in
     KnFundecl
       {
         fn_name = app;
         ty_vars =
           [ (ctrl, KArrow (KType, KType)); (alpha, KType); (beta, KType) ];
         parameters = [ (fs, ty_ctrl_fn); (xs, ty_ctrl_alpha) ];
         return_type = ty_ctrl_beta;
         body = { statements = []; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let beta = TyIdent.fresh "'b" in
     let ctrl = TyIdent.fresh "#t" in
     let ty_alpha = Ty.(v alpha) in
     let ty_beta = Ty.(v beta) in
     let ty_ctrl_alpha = Ty.(varapp ctrl ty_alpha) in
     let ty_ctrl_beta = Ty.(varapp ctrl ty_beta) in
     let ty_fn = Ty.(fn [] [ ty_alpha ] ty_beta) in
     let f = TermIdent.fresh "f" in
     let xs = TermIdent.fresh "xs" in
     let expression =
       Expression.let_plus "x" Ty.(v ctrl) ty_beta Expression.(v xs) []
       @@ fun x _ -> ([], Expression.term_call f [] Expression.[ v x ])
     in
     KnFundecl
       {
         fn_name = map;
         ty_vars =
           [ (ctrl, KArrow (KType, KType)); (alpha, KType); (beta, KType) ];
         parameters = [ (f, ty_fn); (xs, ty_ctrl_alpha) ];
         return_type = ty_ctrl_beta;
         body = { statements = []; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let beta = TyIdent.fresh "'b" in
     let charly = TyIdent.fresh "'c" in
     let ctrl = TyIdent.fresh "#t" in
     let ty_alpha = Ty.(v alpha) in
     let ty_beta = Ty.(v beta) in
     let ty_charly = Ty.(v charly) in
     let ty_ctrl_alpha = Ty.(varapp ctrl ty_alpha) in
     let ty_ctrl_beta = Ty.(varapp ctrl ty_beta) in
     let ty_ctrl_charly = Ty.(varapp ctrl ty_charly) in
     let ty_fn = Ty.(fn [] [ ty_alpha; ty_beta ] ty_charly) in
     let f = TermIdent.fresh "f" in
     let xs = TermIdent.fresh "xs" in
     let ys = TermIdent.fresh "ys" in
     let expression =
       Expression.let_plus "x"
         Ty.(v ctrl)
         ty_charly
         Expression.(v xs)
         Expression.[ ("y", v ys) ]
       @@ fun x ands ->
       let y = match ands with [] -> assert false | t :: _ -> t in
       ([], Expression.term_call f [] Expression.[ v x; v y ])
     in
     KnFundecl
       {
         fn_name = map2;
         ty_vars =
           [
             (ctrl, KArrow (KType, KType));
             (alpha, KType);
             (beta, KType);
             (charly, KType);
           ];
         parameters = [ (f, ty_fn); (xs, ty_ctrl_alpha); (ys, ty_ctrl_beta) ];
         return_type = ty_ctrl_charly;
         body = { statements = []; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let lhs = TermIdent.fresh "lhs" in
     let rhs = TermIdent.fresh "rhs" in
     KnFundecl
       {
         fn_name = fxor;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (lhs, ty_alpha); (rhs, ty_alpha) ];
         return_type = ty_alpha;
         body = { statements = []; expression = Expression.(v lhs lxor v rhs) };
       });
    (let s = TermIdent.fresh "s" in
     let alpha = TyIdent.fresh "'a" in
     let ty_slice = Ty.(app slice (v alpha)) in
     let statements, expression =
       Statement.decl "s0" (Expression.indexing s slice 0) @@ fun s0 ->
       Statement.decl "s1" (Expression.indexing s slice 1) @@ fun s1 ->
       Statement.decl "s2" (Expression.indexing s slice 2) @@ fun s2 ->
       Statement.decl "s3" (Expression.indexing s slice 3) @@ fun s3 ->
       Statement.decl "s1" Expression.(v s1 lxor (v s0 land v s2)) @@ fun s1 ->
       Statement.decl "s0" Expression.(v s0 lxor (v s1 land v s3)) @@ fun s0 ->
       Statement.decl "s2" Expression.(v s2 lxor (v s0 lor v s1)) @@ fun s2 ->
       Statement.decl "s3" Expression.(v s3 lxor v s2) @@ fun s3 ->
       Statement.decl "s1" Expression.(v s1 lxor v s3) @@ fun s1 ->
       Statement.decl "s3" Expression.(lnot (v s3)) @@ fun s3 ->
       Statement.decl "s2" Expression.(v s2 lxor (v s0 land v s1)) @@ fun s2 ->
       Statement.cstr "s" ty_slice Expression.[ v s3; v s1; v s2; v s0 ]
       @@ fun s -> ([], Expression.v s)
     in
     KnFundecl
       {
         fn_name = subcells;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (s, ty_slice) ];
         return_type = ty_slice;
         body = { statements; expression };
       });
    (let s = TermIdent.fresh "s" in
     let key = TermIdent.fresh "key" in
     let alpha = TyIdent.fresh "'a" in
     (* let ty_alpha = Ty.(v alpha) in *)
     let ty_slice = Ty.(app slice (v alpha)) in
     let statements, expression = ([], Expression.(v s lxor v key)) in
     KnFundecl
       {
         fn_name = add_round_key;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (s, ty_slice); (key, ty_slice) ];
         return_type = ty_slice;
         body = { statements; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_col_alpha = Ty.(app col ty_alpha) in
     let ty_cols_rows = Ty.(app col (app row ty_alpha)) in
     let ty_rows_cols = Ty.(app row ty_col_alpha) in
     let state = TermIdent.fresh "s" in
     let index icol irow =
       Expression.(e_indexing (indexing state col icol) row irow)
     in
     let statements, expression =
       Statement.cstr "c0" ty_col_alpha
         [ index 0 0; index 0 1; index 0 2; index 0 3 ]
       @@ fun c0 ->
       Statement.cstr "c1" ty_col_alpha
         [ index 1 0; index 1 1; index 1 2; index 1 3 ]
       @@ fun c1 ->
       Statement.cstr "c2" ty_col_alpha
         [ index 2 0; index 2 1; index 2 2; index 2 3 ]
       @@ fun c2 ->
       Statement.cstr "c3" ty_col_alpha
         [ index 3 0; index 3 1; index 3 2; index 3 3 ]
       @@ fun c3 ->
       Statement.cstr "r" ty_rows_cols Expression.(vars [ c0; c1; c2; c3 ])
       @@ fun r -> ([], Expression.v r)
     in
     KnFundecl
       {
         fn_name = transpose;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (state, ty_cols_rows) ];
         return_type = ty_rows_cols;
         body = { statements; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_row_alpha = Ty.(app row ty_alpha) in
     let ty_row_cols = Ty.(app row (app col ty_alpha)) in
     let ty_col_rows = Ty.(app col ty_row_alpha) in
     let state = TermIdent.fresh "s" in
     let index irow icol =
       Expression.(e_indexing (indexing state row irow) col icol)
     in
     let statements, expression =
       Statement.cstr "r0" ty_row_alpha
         [ index 0 0; index 0 1; index 0 2; index 0 3 ]
       @@ fun r0 ->
       Statement.cstr "r1" ty_row_alpha
         [ index 1 0; index 1 1; index 1 2; index 1 3 ]
       @@ fun r1 ->
       Statement.cstr "r2" ty_row_alpha
         [ index 2 0; index 2 1; index 2 2; index 2 3 ]
       @@ fun r2 ->
       Statement.cstr "r3" ty_row_alpha
         [ index 3 0; index 3 1; index 3 2; index 3 3 ]
       @@ fun r3 ->
       Statement.cstr "c" ty_col_rows Expression.(vars [ r0; r1; r2; r3 ])
       @@ fun c -> ([], Expression.v c)
     in
     KnFundecl
       {
         fn_name = reindex_cols_row;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (state, ty_row_cols) ];
         return_type = ty_col_rows;
         body = { statements; expression };
       });
    (let rows = TermIdent.fresh "rows" in
     let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_rows = Ty.(app row ty_alpha) in
     let statements, expression = ([], Expression.v rows) in
     KnFundecl
       {
         fn_name = row_ror_0;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (rows, ty_rows) ];
         return_type = ty_rows;
         body = { statements; expression };
       });
    (let rows = TermIdent.fresh "rows" in
     let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_rows = Ty.(app row ty_alpha) in
     let index index = Expression.indexing rows row index in
     let statements, expression =
       Statement.cstr "rows" ty_rows [ index 3; index 0; index 1; index 2 ]
       @@ fun rows -> ([], Expression.v rows)
     in
     KnFundecl
       {
         fn_name = row_ror_1;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (rows, ty_rows) ];
         return_type = ty_rows;
         body = { statements; expression };
       });
    (let rows = TermIdent.fresh "rows" in
     let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_rows = Ty.(app row ty_alpha) in
     let index index = Expression.indexing rows row index in
     let statements, expression =
       Statement.cstr "rows" ty_rows [ index 2; index 3; index 0; index 1 ]
       @@ fun rows -> ([], Expression.v rows)
     in
     KnFundecl
       {
         fn_name = row_ror_2;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (rows, ty_rows) ];
         return_type = ty_rows;
         body = { statements; expression };
       });
    (let rows = TermIdent.fresh "rows" in
     let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_rows = Ty.(app row ty_alpha) in
     let index index = Expression.indexing rows row index in
     let statements, expression =
       Statement.cstr "rows" ty_rows [ index 1; index 2; index 3; index 0 ]
       @@ fun rows -> ([], Expression.v rows)
     in
     KnFundecl
       {
         fn_name = row_ror_3;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (rows, ty_rows) ];
         return_type = ty_rows;
         body = { statements; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_cols_rows = Ty.(app col (app row ty_alpha)) in
     let cols = TermIdent.fresh "cols" in
     let ( |> ) e fn_ident = Expression.( |> ) e [ ty_alpha ] fn_ident in

     let statements, expression =
       ( [],
         Expression.v cols |> col_reverse |> transpose |> row_ror_1
         |> reindex_cols_row )
     in
     KnFundecl
       {
         fn_name = rev_rotate_0;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (cols, ty_cols_rows) ];
         return_type = ty_cols_rows;
         body = { statements; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_cols_rows = Ty.(app col (app row ty_alpha)) in
     let cols = TermIdent.fresh "cols" in
     let ( |> ) e fn_ident = Expression.( |> ) e [ ty_alpha ] fn_ident in

     let statements, expression =
       ( [],
         Expression.v cols |> col_reverse |> transpose |> row_ror_2
         |> reindex_cols_row )
     in
     KnFundecl
       {
         fn_name = rev_rotate_1;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (cols, ty_cols_rows) ];
         return_type = ty_cols_rows;
         body = { statements; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_cols_rows = Ty.(app col (app row ty_alpha)) in
     let cols = TermIdent.fresh "cols" in
     let ( |> ) e fn_ident = Expression.( |> ) e [ ty_alpha ] fn_ident in

     let statements, expression =
       ( [],
         Expression.v cols |> col_reverse |> transpose |> row_ror_3
         |> reindex_cols_row )
     in
     KnFundecl
       {
         fn_name = rev_rotate_2;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (cols, ty_cols_rows) ];
         return_type = ty_cols_rows;
         body = { statements; expression };
       });
    (let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_cols_rows = Ty.(app col @@ app row ty_alpha) in
     let cols = TermIdent.fresh "cols" in
     let ( |> ) e fn_ident = Expression.( |> ) e [ ty_alpha ] fn_ident in
     let statements, expression =
       ( [],
         Expression.v cols |> col_reverse |> transpose |> row_ror_0
         |> reindex_cols_row )
     in
     KnFundecl
       {
         fn_name = rev_rotate_3;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (cols, ty_cols_rows) ];
         return_type = ty_cols_rows;
         body = { statements; expression };
       });
    (let cols = TermIdent.fresh "cols" in
     let alpha = TyIdent.fresh "'a" in
     let ty_alpha = Ty.(v alpha) in
     let ty_col_alpha = Ty.(app col ty_alpha) in
     let index index = Expression.indexing cols col index in
     let statements, expression =
       Statement.cstr "cols" ty_col_alpha [ index 3; index 2; index 1; index 0 ]
       @@ fun cols -> ([], Expression.v cols)
     in
     KnFundecl
       {
         fn_name = col_reverse;
         ty_vars = [ (alpha, KType) ];
         parameters = [ (cols, ty_col_alpha) ];
         return_type = ty_col_alpha;
         body = { statements; expression };
       });
    (let state' = TermIdent.fresh "state" in
     let key = TermIdent.fresh "key" in
     (*     let ty_alpha = Ty.(v alpha) in*)
     let ty_state = Ty.(eapp state) in
     let ty_cols_rows = Ty.(app col @@ app row bool) in
     let ty_fn_row_cols__row_cols = Ty.(fn [] [ ty_cols_rows ] ty_cols_rows) in
     let ty_cols_rows_bool = Ty.(app col @@ app row bool) in
     let ty_cols_rows_partial = Ty.(app col @@ eapp row) in
     let ty_slice = Ty.(app slice ty_fn_row_cols__row_cols) in
     let statements, expression =
       Statement.cstr "permbits" ty_slice
         Expression.
           [
             fv_t rev_rotate_1 [ Ty.bool ];
             fv_t rev_rotate_2 [ Ty.bool ];
             fv_t rev_rotate_3 [ Ty.bool ];
             fv_t rev_rotate_0 [ Ty.bool ];
           ]
       @@ fun permbits ->
       Statement.decl "state"
         Expression.(
           let_plus "slice" ty_cols_rows_partial
             Ty.(app slice bool)
             Expression.(v state')
             []
           @@ fun expr _ ->
           ([], fn_call subcells [ Ty.(app slice bool) ] [ v expr ]))
       @@ fun state ->
       Statement.log [ state ] @@ fun () ->
       Statement.decl "state"
         Expression.(
           fn_call app
             Ty.[ ty_cols_rows_partial; bool; bool ]
             [ v permbits; v state ])
       @@ fun state ->
       Statement.log [ state ] @@ fun () ->
       ( [],
         Expression.(
           fn_call add_round_key [ ty_cols_rows_bool ] [ v state; v key ]) )
     in
     KnFundecl
       {
         fn_name = round;
         ty_vars = [];
         parameters = [ (state', ty_state); (key, ty_state) ];
         return_type = ty_state;
         body = { statements; expression };
       });
    (let vstate = TermIdent.fresh "state" in
     let vkeys = TermIdent.fresh "keys" in
     let ty_state = Ty.(eapp state) in
     let ty_keys = Ty.(eapp keys) in
     let expression =
       List.init 1 Fun.id
       |> List.fold_left
            (fun acc i ->
              Expression.(fn_call round [] [ acc; indexing vkeys keys i ]))
            (Expression.v vstate)
     in
     KnFundecl
       {
         fn_name = fngift;
         ty_vars = [];
         parameters = [ (vstate, ty_state); (vkeys, ty_keys) ];
         return_type = ty_state;
         body = { statements = []; expression };
       });
  ]
