open MaoAst
open MaoAst_pp
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
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType IntType, BaseType IntType ->
      Result.ok (BaseType BoolType)
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType DoubleType, BaseType DoubleType
    ->
      Result.ok (BaseType BoolType)
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType IntType, wrong ->
      type_error ~loc:e2_loc (BaseType IntType) wrong
  | (Eq | Neq | Lt | Gt | Leq | Geq), BaseType DoubleType, wrong ->
      type_error ~loc:e2_loc (BaseType DoubleType) wrong
  | (Eq | Neq | Lt | Gt | Leq | Geq), wrong, _ ->
      type_error ~loc:e1_loc (BaseType IntType) wrong

let check_un_op ~e_loc op t =
  match (op, t) with
  | Neg, BaseType IntType -> Result.ok (BaseType IntType)
  | Not, BaseType BoolType -> Result.ok (BaseType BoolType)
  | Neg, wrong -> type_error ~loc:e_loc (BaseType IntType) wrong
  | Not, wrong -> type_error ~loc:e_loc (BaseType BoolType) wrong

let rec type_check_expr env Loc.{ it = e; at = loc } =
  match e with
  | Val { it = IntVal _ } -> Result.ok (BaseType IntType)
  | Val { it = BoolVal _ } -> Result.ok (BaseType BoolType)
  | Val { it = DoubleVal _ } -> Result.ok (BaseType DoubleType)
  | Val { it = CharVal _ } -> Result.ok (BaseType CharType)
  | Var id -> (
      match Env.find_opt id env with
      | Some t -> Result.ok t
      | None -> undefined_variable_error ~loc id)
  | BinOp (e1, op, e2) ->
      let* t1 = type_check_expr env e1 in
      let* t2 = type_check_expr env e2 in
      check_bin_op ~e1_loc:e1.at ~e2_loc:e2.at op t1 t2
  | UnOp (op, e) ->
      let* t = type_check_expr env e in
      check_un_op ~e_loc:e.at op t
  | ArrayLoad (id, e) -> (
      match Env.find_opt id env with
      | Some (CompositeType (Array t)) -> (
          let* t = type_check_expr env e in
          match t with
          | BaseType IntType -> Result.ok t
          | _ -> type_error ~loc (BaseType IntType) t)
      | Some t -> expected_array_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | NewArrayDefault (t, e) -> (
      let* t' = type_check_expr env e in
      match t' with
      | BaseType IntType -> Result.ok (CompositeType (Array t))
      | _ -> type_error ~loc (BaseType IntType) t')
  | NewArrayLiteral [] ->
      error_with_loc ~loc "cannot create an empty literal array"
  | NewArrayLiteral (v :: vs) -> (
      let+ t =
        List.fold_left
          (fun acc v ->
            let* t = acc in
            let* t' = type_check_expr env { it = Val v; at = v.at } in
            if t = t' then Result.ok t else type_error ~loc:v.at t t')
          (type_check_expr env { it = Val v; at = v.at })
          vs
      in
      match t with
      | BaseType t -> CompositeType (Array t)
      | _ -> unreachable "non-base type in array literal")
  | ArrayLength id -> (
      match Env.find_opt id env with
      | Some (CompositeType (Array _)) -> Result.ok (BaseType IntType)
      | Some t -> expected_array_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | Cast (t, e) -> (
      let* t' = type_check_expr env e in
      match (t, t') with
      | BaseType IntType, BaseType DoubleType
      | BaseType DoubleType, BaseType IntType ->
          Result.ok t
      | _ -> cast_error ~loc t' t)
  | Call (id, args) -> (
      match Env.find_opt id env with
      | Some (FuncType (ts, Some t)) ->
          if List.length ts <> List.length args then
            error_with_loc ~loc "Function '%s' expects %d arguments but got %d"
              id (List.length ts) (List.length args)
          else
            let* _ =
              List.fold_left2
                (fun acc formal_t actual_e ->
                  let* () = acc in
                  let* actual_t = type_check_expr env actual_e in
                  if formal_t = actual_t then Result.ok ()
                  else type_error ~loc:actual_e.at formal_t actual_t)
                (Result.ok ()) ts args
            in
            Result.ok t
      | Some (FuncType (ts, None)) ->
          expected_function_error ~loc id (FuncType (ts, None))
      | Some t -> expected_function_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | ExprBlock (c, e) ->
      let* env' = type_check_command env c in
      type_check_expr env' e
  | Ternary (e1, e2, e3) ->
      let* t1 = type_check_expr env e1 in
      if t1 <> BaseType BoolType then type_error ~loc (BaseType BoolType) t1
      else
        let* t2 = type_check_expr env e2 in
        let* t3 = type_check_expr env e3 in
        if t2 = t3 then Result.ok t2 else type_error ~loc:e3.at t2 t3

and type_check_command env { it = c; at = loc } =
  match c with
  | Skip -> Result.ok env
  | Declare (ds, None) ->
      let* env' =
        result_foldM
          (fun acc (t, id) ->
            match t with
            | BaseType _ -> Result.ok (Env.add id t acc)
            | _ ->
                error_with_loc ~loc
                  "array type '%a' cannot be declared without an initializer"
                  pp_mao_type t)
          env ds
      in
      Result.ok env'
  | Declare (ds, Some es) ->
      if List.length ds <> List.length es then
        error_with_loc ~loc "expected %d expressions but got %d"
          (List.length ds) (List.length es)
      else
        let* env =
          List.fold_left2
            (fun acc (t, id) e ->
              let* env = acc in
              let* t' = type_check_expr env e in
              if t = t' then Result.ok (Env.add id t env)
              else type_error ~loc:e.at t t')
            (Result.ok env) ds es
        in
        Result.ok env
  | Assign (ids, es) ->
      if List.length ids <> List.length es then
        error_with_loc ~loc "expected %d expressions but got %d"
          (List.length ids) (List.length es)
      else
        let* _ =
          List.fold_left2
            (fun acc id e ->
              let* env = acc in
              match Env.find_opt id env with
              | Some t ->
                  let* t' = type_check_expr env e in
                  if t = t' then Result.ok env else type_error ~loc:e.at t t'
              | None -> undefined_variable_error ~loc id)
            (Result.ok env) ids es
        in
        Result.ok env
  | ArrayStore (id, e1, e2) -> (
      match Env.find_opt id env with
      | Some (CompositeType (Array t)) ->
          let* t1 = type_check_expr env e1 in
          let* t2 = type_check_expr env e2 in
          if t1 <> BaseType IntType then
            type_error ~loc:e1.at (BaseType IntType) t1
          else if t2 <> BaseType t then type_error ~loc:e2.at (BaseType t) t2
          else Result.ok env
      | Some t -> expected_array_error ~loc id t
      | None -> undefined_variable_error ~loc id)
  | Seq (c1, c2) ->
      let* env' = type_check_command env c1 in
      type_check_command env' c2
  | IfThenElse (e, c1, c2) ->
      let* t = type_check_expr env e in
      if t <> BaseType BoolType then type_error ~loc (BaseType BoolType) t
      else
        let* _ = type_check_command env c1 in
        let* _ = type_check_command env c2 in
        Result.ok env
  | While (e, c) ->
      let* t = type_check_expr env e in
      if t <> BaseType BoolType then type_error ~loc (BaseType BoolType) t
      else type_check_command env c
  | Block c ->
      let* _ = type_check_command env c in
      Result.ok env
  | Proc (id, ds, c) ->
      let env' = Env.add id (FuncType (List.map fst ds, None)) env in
      let env'' =
        List.fold_left (fun acc (t, id) -> Env.add id t acc) env' ds
      in
      let* _ = type_check_command env'' c in
      Result.ok env'
  | Invoke (id, es) -> (
      match Env.find_opt id env with
      | Some (FuncType (ts, None)) ->
          if List.length ts <> List.length es then
            error_with_loc ~loc "procedure '%s' expects %d arguments but got %d"
              id (List.length ts) (List.length es)
          else
            let* _ =
              List.fold_left2
                (fun acc t e ->
                  let* () = acc in
                  let* t' = type_check_expr env e in
                  if t = t' then Result.ok () else type_error ~loc:e.at t t')
                (Result.ok ()) ts es
            in
            Result.ok env
      | Some t ->
          error_with_loc ~loc "expected '%s' to be a procedure but got %a" id
            pp_mao_type t
      | None -> undefined_variable_error ~loc id)
  | Func (id, ds, t, e) ->
      let env' = Env.add id (FuncType (List.map fst ds, Some t)) env in
      let env'' =
        List.fold_left (fun acc (t, id) -> Env.add id t acc) env' ds
      in
      let* t' = type_check_expr env'' e in
      if t = t' then Result.ok env' else type_error ~loc:e.at t t'

let type_check_program ast =
  let+ _ = type_check_command Utils.Env.empty ast in
  ()
