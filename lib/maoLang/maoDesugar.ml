open MaoAst
open Utils

let step_assign ~loc op base_type id =
  let ll x = Loc.with_loc ~loc x in
  let one =
    match base_type with
    | IntType -> ll @@ Val (ll @@ IntVal 1L)
    | DoubleType -> ll @@ Val (ll @@ DoubleVal 1.0)
    | _ -> unreachable "step_assign: unsupported type"
  in
  let bin_op = match op with Inc -> Add | Dec -> Sub in
  ll @@ Assign ([ id ], [ ll @@ BinOp (ll @@ Var id, bin_op, one) ])

let desugar_expr Loc.{ it = e; at = loc } =
  let ll x = Loc.with_loc ~loc x in
  match e with
  | SugaredExpr (Pre, op, base_type, id) ->
      ll @@ ExprBlock (step_assign ~loc op base_type id, ll @@ Var id)
  | SugaredExpr (Post, op, base_type, id) ->
      let tmp_id = "_tmp_" ^ id in
      let save_in_tmp =
        ll @@ Declare ([ (BaseType base_type, tmp_id) ], Some [ ll @@ Var id ])
      in
      let assign = step_assign ~loc op base_type id in
      ll @@ ExprBlock (ll @@ Seq (save_in_tmp, assign), ll @@ Var tmp_id)
  | _ -> unreachable "desugar_expr: not a sugared expression"

let desugar_command Loc.{ it = c; at = loc } =
  let ll x = Loc.with_loc ~loc x in
  match c with
  | SugaredCmd (op, base_type, id) -> step_assign ~loc op base_type id
  | SugaredCmdAssign (id, bin_op, e) ->
      ll @@ Assign ([ id ], [ ll @@ BinOp (ll @@ Var id, bin_op, e) ])
  | _ -> unreachable "desugar_command: not a sugared command"
