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

let step_expr ~loc fix op base_type id =
  let ll x = Loc.with_loc ~loc x in
  let assign = step_assign ~loc op base_type id in
  match fix with
  | Pre -> ll @@ ExprBlock (assign, ll @@ Var id)
  | Post ->
      let tmp_id = "_tmp_" ^ id in
      let save_in_tmp =
        ll @@ Declare ([ (BaseType base_type, tmp_id) ], Some [ ll @@ Var id ])
      in
      ll @@ ExprBlock (ll @@ Seq (save_in_tmp, assign), ll @@ Var tmp_id)

let compound_assign ~loc id bin_op e =
  let ll x = Loc.with_loc ~loc x in
  ll @@ Assign ([ id ], [ ll @@ BinOp (ll @@ Var id, bin_op, e) ])
