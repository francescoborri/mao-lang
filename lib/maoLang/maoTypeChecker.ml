open MaoAst
open MaoAst_pp
open MaoDataStructure
open Utils
open Result.Syntax

let error_with_loc ~loc fmt = Fmt.kstr (Loc.with_loc ~loc >> Result.error) fmt

let undefined_variable_error ~loc id =
  error_with_loc ~loc "undefined variable '%s'" id

let type_error ~loc expected got =
  error_with_loc ~loc "expected %a but got %a" pp_mao_type expected pp_mao_type
    got

let expected_array_error ~loc id t =
  error_with_loc ~loc "expected '%s' to be an array but got %a" id pp_mao_type t

let expected_function_error ~loc id t =
  error_with_loc ~loc "expected '%s' to be a function but got %a" id pp_mao_type
    t

let cast_error ~loc t1 t2 =
  error_with_loc ~loc "cannot cast from %a to %a" pp_mao_type t1 pp_mao_type t2

let default_value ~loc = function
  | IntType -> Loc.with_loc ~loc (CVal { it = IntVal 0; at = loc })
  | BoolType -> Loc.with_loc ~loc (CVal { it = BoolVal false; at = loc })
  | DoubleType -> Loc.with_loc ~loc (CVal { it = DoubleVal 0.0; at = loc })
  | CharType -> Loc.with_loc ~loc (CVal { it = CharVal '\000'; at = loc })

let check_bin_op ~e1_loc ~e2_loc op t1 t2 =
  match (op, t1, t2) with
  | (Add | Sub | Mul | Div | Mod), BaseType IntType, BaseType IntType ->
      Result.ok (BaseType IntType)
  | (Add | Sub | Mul | Div | Mod), BaseType DoubleType, BaseType DoubleType ->
      Result.ok (BaseType DoubleType)
  | (Add | Sub | Mul | Div | Mod), BaseType IntType, wrong ->
      type_error ~loc:e2_loc (BaseType IntType) wrong
  | (Add | Sub | Mul | Div | Mod), BaseType DoubleType, wrong ->
      type_error ~loc:e2_loc (BaseType DoubleType) wrong
  | (Add | Sub | Mul | Div | Mod), wrong, _ ->
      type_error ~loc:e1_loc (BaseType IntType) wrong
  | (And | Or | Xor), BaseType BoolType, BaseType BoolType ->
      Result.ok (BaseType BoolType)
  | (And | Or | Xor), BaseType BoolType, wrong ->
      type_error ~loc:e2_loc (BaseType BoolType) wrong
  | (And | Or | Xor), wrong, _ ->
      type_error ~loc:e1_loc (BaseType BoolType) wrong
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType IntType, BaseType IntType
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType DoubleType, BaseType DoubleType
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType CharType, BaseType CharType ->
      Result.ok (BaseType BoolType)
  | (Eq | Neq), BaseType BoolType, BaseType BoolType ->
      Result.ok (BaseType BoolType)
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType IntType, wrong ->
      type_error ~loc:e2_loc (BaseType IntType) wrong
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType DoubleType, wrong ->
      type_error ~loc:e2_loc (BaseType DoubleType) wrong
  | (Eq | Neq), BaseType BoolType, wrong ->
      type_error ~loc:e2_loc (BaseType BoolType) wrong
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType CharType, wrong ->
      type_error ~loc:e2_loc (BaseType CharType) wrong
  | (Eq | Neq | Lt | Gt | Leq | Geq), wrong, _ ->
      type_error ~loc:e1_loc (BaseType IntType) wrong

let check_un_op ~e_loc op t =
  match (op, t) with
  | Neg, BaseType IntType -> Result.ok (BaseType IntType)
  | Neg, BaseType DoubleType -> Result.ok (BaseType DoubleType)
  | Not, BaseType BoolType -> Result.ok (BaseType BoolType)
  | Neg, wrong -> type_error ~loc:e_loc (BaseType IntType) wrong
  | Not, wrong -> type_error ~loc:e_loc (BaseType BoolType) wrong

let rec type_check_expr env Loc.{ it = e; at = loc } =
  match e with
  | Val ({ it = IntVal _ } as v) ->
      Result.ok (BaseType IntType, Loc.with_loc ~loc (CVal v))
  | Val ({ it = BoolVal _ } as v) ->
      Result.ok (BaseType BoolType, Loc.with_loc ~loc (CVal v))
  | Val ({ it = DoubleVal _ } as v) ->
      Result.ok (BaseType DoubleType, Loc.with_loc ~loc (CVal v))
  | Val ({ it = CharVal _ } as v) ->
      Result.ok (BaseType CharType, Loc.with_loc ~loc (CVal v))
  | Var id -> (
      match Env.find_opt id env with
      | Some t -> Result.ok (t, Loc.with_loc ~loc (CVar id))
      | None -> undefined_variable_error ~loc id)
  | BinOp (e1, op, e2) ->
      let* t1, c_e1 = type_check_expr env e1 in
      let* t2, c_e2 = type_check_expr env e2 in
      let* t = check_bin_op ~e1_loc:e1.at ~e2_loc:e2.at op t1 t2 in
      Result.ok (t, Loc.with_loc ~loc (CBinOp (c_e1, op, c_e2)))
  | UnOp (op, e) ->
      let* t, c_e = type_check_expr env e in
      let* t' = check_un_op ~e_loc:e.at op t in
      Result.ok (t', Loc.with_loc ~loc (CUnOp (op, c_e)))
  | ArrayLoad (id, e) -> (
      let* t =
        match Env.find_opt id env with
        | Some (CompositeType (Array t)) -> Result.ok (BaseType t)
        | Some t -> expected_array_error ~loc id t
        | None -> undefined_variable_error ~loc id
      in
      let* t', c_e = type_check_expr env e in
      match t' with
      | BaseType IntType ->
          Result.ok (t, Loc.with_loc ~loc (CArrayLoad (id, c_e)))
      | _ -> type_error ~loc (BaseType IntType) t')
  | NewArrayDefault (t, e) ->
      let* t', c_e = type_check_expr env e in
      let* () =
        match t' with
        | BaseType IntType -> Result.ok ()
        | _ -> type_error ~loc:e.at (BaseType IntType) t'
      in
      Result.ok
        (CompositeType (Array t), Loc.with_loc ~loc (CNewArrayDefault (t, c_e)))
  | NewArrayLiteral [] ->
      error_with_loc ~loc "cannot create an empty literal array"
  | NewArrayLiteral (v :: vs) ->
      let* v_t, _ = type_check_expr env { it = Val v; at = v.at } in
      let+ t =
        List.fold_left
          (fun acc v ->
            let* t = acc in
            let* t', _ = type_check_expr env { it = Val v; at = v.at } in
            if t = t' then Result.ok t else type_error ~loc:v.at t t')
          (Result.ok v_t) vs
      in
      let base_t =
        match t with
        | BaseType t -> t
        | _ -> unreachable "non-base type in array literal"
      in
      ( CompositeType (Array base_t),
        Loc.with_loc ~loc (CNewArrayLiteral (v :: vs)) )
  | ArrayLength id -> (
      match Env.find_opt id env with
      | Some (CompositeType (Array _)) ->
          Result.ok (BaseType IntType, Loc.with_loc ~loc (CArrayLength id))
      | Some t -> expected_array_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | Cast (t, e) -> (
      let* t', c_e = type_check_expr env e in
      match (t, t') with
      | BaseType IntType, BaseType DoubleType
      | BaseType DoubleType, BaseType IntType ->
          Result.ok (t, Loc.with_loc ~loc (CCast (t, c_e)))
      | _ -> cast_error ~loc t' t)
  | Call (id, args) -> (
      match Env.find_opt id env with
      | Some (FuncType (ts, Some t)) when List.length ts = List.length args ->
          let c_args = List.map (type_check_expr env) args in
          let* c_args =
            result_fold_left2M
              (fun acc t e ->
                let* t', c_e = e in
                if t = t' then Result.ok (c_e :: acc)
                else type_error ~loc:c_e.Loc.at t t')
              [] ts c_args
          in
          let c_args = List.rev c_args in
          Result.ok (t, Loc.with_loc ~loc (CCall (id, c_args)))
      | Some (FuncType (ts, Some t)) when List.length ts <> List.length args ->
          error_with_loc ~loc "function '%s' expects %d arguments but got %d" id
            (List.length ts) (List.length args)
      | Some (FuncType (ts, None)) ->
          expected_function_error ~loc id (FuncType (ts, None))
      | Some t -> expected_function_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | ExprBlock (c, e) ->
      let* env', c_c = type_check_command env c in
      let* t, c_e = type_check_expr env' e in
      Result.ok (t, Loc.with_loc ~loc (CExprBlock (c_c, c_e)))
  | Ternary (e1, e2, e3) ->
      let* t1, c_e1 = type_check_expr env e1 in
      if t1 <> BaseType BoolType then type_error ~loc (BaseType BoolType) t1
      else
        let* t2, c_e2 = type_check_expr env e2 in
        let* t3, c_e3 = type_check_expr env e3 in
        if t2 = t3 then
          Result.ok (t2, Loc.with_loc ~loc (CTernary (c_e1, c_e2, c_e3)))
        else type_error ~loc:e3.at t2 t3

and type_check_command env { it = c; at = loc } =
  match c with
  | Skip -> Result.ok (env, Loc.with_loc ~loc CSkip)
  | Declare (ds, None) ->
      let* env', c_es =
        result_fold_leftM
          (fun (env, c_es) (t, id) ->
            match t with
            | BaseType bt ->
                Result.ok (Env.add id t env, default_value ~loc bt :: c_es)
            | _ ->
                error_with_loc ~loc
                  "array type '%a' cannot be declared without an initializer"
                  pp_mao_type t)
          (env, []) ds
      in
      let c_es = List.rev c_es in
      let c_c = CDeclare (List.combine ds c_es) in
      Result.ok (env', Loc.with_loc ~loc c_c)
  | Declare (ds, Some es) when List.length ds = List.length es ->
      let* env, c_es =
        result_fold_left2M
          (fun (env, c_es) (t, id) e ->
            let* t', c_e = type_check_expr env e in
            if t = t' then Result.ok (Env.add id t env, c_e :: c_es)
            else type_error ~loc:e.at t t')
          (env, []) ds es
      in
      let c_es = List.rev c_es in
      let c_c = CDeclare (List.combine ds c_es) in
      Result.ok (env, Loc.with_loc ~loc c_c)
  | Declare (ds, Some es) ->
      error_with_loc ~loc "expected %d expressions but got %d" (List.length ds)
        (List.length es)
  | Assign (ids, es) when List.length ids = List.length es ->
      let* env, c_es =
        result_fold_left2M
          (fun (env, c_es) id e ->
            match Env.find_opt id env with
            | Some t ->
                let* t', c_e = type_check_expr env e in
                if t = t' then Result.ok (env, c_e :: c_es)
                else type_error ~loc:e.at t t'
            | None -> undefined_variable_error ~loc id)
          (env, []) ids es
      in
      let c_es = List.rev c_es in
      let c_c = CAssign (List.combine ids c_es) in
      Result.ok (env, Loc.with_loc ~loc c_c)
  | Assign (ids, es) ->
      error_with_loc ~loc "expected %d expressions but got %d" (List.length ids)
        (List.length es)
  | ArrayStore (id, e1, e2) -> (
      match Env.find_opt id env with
      | Some (CompositeType (Array t)) ->
          let* t1, c_e1 = type_check_expr env e1 in
          let* t2, c_e2 = type_check_expr env e2 in
          if t1 <> BaseType IntType then
            type_error ~loc:e1.at (BaseType IntType) t1
          else if t2 <> BaseType t then type_error ~loc:e2.at (BaseType t) t2
          else Result.ok (env, Loc.with_loc ~loc (CArrayStore (id, c_e1, c_e2)))
      | Some t -> expected_array_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | Seq (c1, c2) ->
      let* env, c_c1 = type_check_command env c1 in
      let* env, c_c2 = type_check_command env c2 in
      Result.ok (env, Loc.with_loc ~loc (CSeq (c_c1, c_c2)))
  | IfThenElse (e, c1, c2) ->
      let* t, c_e = type_check_expr env e in
      if t <> BaseType BoolType then type_error ~loc (BaseType BoolType) t
      else
        let* _, c_c1 = type_check_command env c1 in
        let* _, c_c2 = type_check_command env c2 in
        Result.ok (env, Loc.with_loc ~loc (CIfThenElse (c_e, c_c1, c_c2)))
  | While (e, c) ->
      let* t, c_e = type_check_expr env e in
      if t <> BaseType BoolType then type_error ~loc (BaseType BoolType) t
      else
        let* _, c_c = type_check_command env c in
        Result.ok (env, Loc.with_loc ~loc (CWhile (c_e, c_c)))
  | Block c ->
      let* _, c_c = type_check_command env c in
      Result.ok (env, Loc.with_loc ~loc (CBlock c_c))
  | Proc (id, ds, c) ->
      let env' = Env.add id (FuncType (List.map fst ds, None)) Env.empty in
      let env' = List.fold_left (fun acc (t, id) -> Env.add id t acc) env' ds in
      let* _, c_c = type_check_command env' c in
      let env = Env.add id (FuncType (List.map fst ds, None)) env in
      Result.ok (env, Loc.with_loc ~loc (CProc (id, ds, c_c)))
  | Invoke (id, es) -> (
      match Env.find_opt id env with
      | Some (FuncType (ts, None)) ->
          if List.length ts <> List.length es then
            error_with_loc ~loc "procedure '%s' expects %d arguments but got %d"
              id (List.length ts) (List.length es)
          else
            let* c_es =
              result_fold_left2M
                (fun c_es t e ->
                  let* t', c_e = type_check_expr env e in
                  if t = t' then Result.ok (c_e :: c_es)
                  else type_error ~loc:e.at t t')
                [] ts es
            in
            let c_es = List.rev c_es in
            Result.ok (env, Loc.with_loc ~loc (CInvoke (id, c_es)))
      | Some t ->
          error_with_loc ~loc "expected '%s' to be a procedure but got %a" id
            pp_mao_type t
      | None -> undefined_variable_error ~loc id)
  | Func (id, ds, t, e) ->
      let env' = Env.add id (FuncType (List.map fst ds, Some t)) Env.empty in
      let env' = List.fold_left (fun acc (t, id) -> Env.add id t acc) env' ds in
      let* t', c_e = type_check_expr env' e in
      if t = t' then
        let env = Env.add id (FuncType (List.map fst ds, Some t)) env in
        Result.ok (env, Loc.with_loc ~loc (CFunc (id, ds, t, c_e)))
      else type_error ~loc:e.at t t'

let type_check_program ast =
  let+ _, c_ast = type_check_command Env.empty ast in
  c_ast
