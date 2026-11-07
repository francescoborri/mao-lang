open MaoAst
open MaoAst_pp
open Utils
open WriterResultMonad.Syntax

module IntAddr = struct
  type t = int

  let zero = 0
  let next x = x + 1
  let compare = compare
  let pp = Fmt.int
end

module Mem = Mem.Make (IntAddr)
module WRM = WriterResultMonad

type idk_what_to_call_it =
  | Addr of IntAddr.t
  | LambdaFun of typed_var list * expr
  | LambdaProc of typed_var list * command

type rt_val =
  | RTInt of int
  | RTBool of bool
  | RTDouble of float
  | RTChar of char

type env = idk_what_to_call_it Env.t
type mem = rt_val Mem.t
type log = { event : string; env : env; mem : mem; loc : Loc.loc }

let error_with_loc ~loc fmt = Fmt.kstr (Loc.with_loc ~loc >> WRM.error) fmt

let undefined_variable_error ~loc id =
  error_with_loc ~loc "undefined variable '%s'" id

let type_error ~loc expected got =
  error_with_loc ~loc "expected %a but got %a" pp_mao_type expected pp_mao_type
    got

let expected_array_error ~loc id t =
  error_with_loc ~loc "expected '%s' to be an array but got %a" id pp_mao_type t

let cast_error ~loc t1 t2 =
  error_with_loc ~loc "cannot cast from %a to %a" pp_mao_type t1 pp_mao_type t2

let division_by_zero_error ~loc = error_with_loc ~loc "division by zero"

let undefined_memory_error ~loc addr =
  error_with_loc ~loc "undefined memory at address '%a'" IntAddr.pp addr

let expected_address_error ~loc id =
  error_with_loc ~loc "expected '%s' to have a memory address" id

let expected_function_error ~loc id =
  error_with_loc ~loc "expected '%s' to be a function" id

let expected_procedure_error ~loc id =
  error_with_loc ~loc "expected '%s' to be a procedure" id

let index_out_of_bounds_error ~loc idx arr_len =
  error_with_loc ~loc "index %d out of bounds for array of length %d" idx
    arr_len

let default_value_of_type = function
  | IntType -> RTInt 0
  | BoolType -> RTBool false
  | DoubleType -> RTDouble 0.0
  | CharType -> RTChar '\000'

let type_of_rt_val = function
  | RTInt _ -> IntType
  | RTBool _ -> BoolType
  | RTDouble _ -> DoubleType
  | RTChar _ -> CharType

let cast_int v cont =
  match v with RTInt i -> WRM.ok i | v -> cont (BaseType (type_of_rt_val v))

let cast_bool v cont =
  match v with RTBool b -> WRM.ok b | v -> cont (BaseType (type_of_rt_val v))

let lookup_addr ~loc mem addr =
  match Mem.find_opt addr mem with
  | Some v -> WRM.ok v
  | None -> undefined_memory_error ~loc addr

let lookup_var ~loc env mem id =
  match Env.find_opt id env with
  | Some (Addr addr) -> lookup_addr ~loc mem addr
  | Some (LambdaFun _) -> expected_function_error ~loc id
  | Some (LambdaProc _) -> expected_procedure_error ~loc id
  | None -> undefined_variable_error ~loc id

let store_var ~loc env mem id v =
  match Env.find_opt id env with
  | Some (Addr addr) -> WRM.ok (Mem.add addr v mem)
  | Some (LambdaFun _) -> expected_function_error ~loc id
  | Some (LambdaProc _) -> expected_procedure_error ~loc id
  | None -> undefined_variable_error ~loc id

let lookup_fun ~loc env id =
  match Env.find_opt id env with
  | Some (LambdaFun (params, body)) -> WRM.ok (params, body)
  | Some (Addr _) -> expected_function_error ~loc id
  | Some (LambdaProc _) -> expected_procedure_error ~loc id
  | None -> undefined_variable_error ~loc id

let lookup_proc ~loc env id =
  match Env.find_opt id env with
  | Some (LambdaProc (params, body)) -> WRM.ok (params, body)
  | Some (Addr _) -> expected_procedure_error ~loc id
  | Some (LambdaFun _) -> expected_function_error ~loc id
  | None -> undefined_variable_error ~loc id

let alloc_array data mem =
  let n = List.length data in
  let mem, arr_ptr = Mem.alloc (RTInt n) mem in
  let addrs = List.init n (fun i -> arr_ptr + i + 1) in
  let mem =
    List.fold_left2
      (fun mem addr datum -> Mem.add addr datum mem)
      mem addrs data
  in
  (RTInt arr_ptr, mem)

let eval_bin_op ~e1_loc ~e2_loc op v1 v2 =
  match (op, v1, v2) with
  | Add, RTInt i1, RTInt i2 -> WRM.ok (RTInt (i1 + i2))
  | Sub, RTInt i1, RTInt i2 -> WRM.ok (RTInt (i1 - i2))
  | Mul, RTInt i1, RTInt i2 -> WRM.ok (RTInt (i1 * i2))
  | Div, RTInt i1, RTInt i2 ->
      if i2 = 0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (RTInt (i1 / i2))
  | Mod, RTInt i1, RTInt i2 ->
      if i2 = 0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (RTInt (i1 mod i2))
  | Add, RTDouble d1, RTDouble d2 -> WRM.ok (RTDouble (d1 +. d2))
  | Sub, RTDouble d1, RTDouble d2 -> WRM.ok (RTDouble (d1 -. d2))
  | Mul, RTDouble d1, RTDouble d2 -> WRM.ok (RTDouble (d1 *. d2))
  | Div, RTDouble d1, RTDouble d2 ->
      if d2 = 0.0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (RTDouble (d1 /. d2))
  | Mod, RTDouble d1, RTDouble d2 ->
      if d2 = 0.0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (RTDouble (mod_float d1 d2))
  | (Add | Sub | Mul | Div | Mod), RTInt _, v ->
      type_error ~loc:e2_loc (BaseType IntType) (BaseType (type_of_rt_val v))
  | (Add | Sub | Mul | Div | Mod), RTDouble _, v ->
      type_error ~loc:e2_loc (BaseType DoubleType) (BaseType (type_of_rt_val v))
  | (Add | Sub | Mul | Div | Mod), v, _ ->
      type_error ~loc:e1_loc (BaseType IntType) (BaseType (type_of_rt_val v))
  | And, RTBool b1, RTBool b2 -> WRM.ok (RTBool (b1 && b2))
  | Or, RTBool b1, RTBool b2 -> WRM.ok (RTBool (b1 || b2))
  | Xor, RTBool b1, RTBool b2 -> WRM.ok (RTBool (b1 <> b2))
  | (And | Or | Xor), RTBool _, v ->
      type_error ~loc:e2_loc (BaseType BoolType) (BaseType (type_of_rt_val v))
  | (And | Or | Xor), _, v ->
      type_error ~loc:e1_loc (BaseType BoolType) (BaseType (type_of_rt_val v))
  | Eq, RTInt i1, RTInt i2 -> WRM.ok (RTBool (i1 = i2))
  | Neq, RTInt i1, RTInt i2 -> WRM.ok (RTBool (i1 <> i2))
  | Lt, RTInt i1, RTInt i2 -> WRM.ok (RTBool (i1 < i2))
  | Gt, RTInt i1, RTInt i2 -> WRM.ok (RTBool (i1 > i2))
  | Leq, RTInt i1, RTInt i2 -> WRM.ok (RTBool (i1 <= i2))
  | Geq, RTInt i1, RTInt i2 -> WRM.ok (RTBool (i1 >= i2))
  | Eq, RTDouble d1, RTDouble d2 -> WRM.ok (RTBool (d1 = d2))
  | Neq, RTDouble d1, RTDouble d2 -> WRM.ok (RTBool (d1 <> d2))
  | Lt, RTDouble d1, RTDouble d2 -> WRM.ok (RTBool (d1 < d2))
  | Gt, RTDouble d1, RTDouble d2 -> WRM.ok (RTBool (d1 > d2))
  | Leq, RTDouble d1, RTDouble d2 -> WRM.ok (RTBool (d1 <= d2))
  | Geq, RTDouble d1, RTDouble d2 -> WRM.ok (RTBool (d1 >= d2))
  | (Eq | Neq | Lt | Gt | Leq | Geq), RTInt _, v ->
      type_error ~loc:e2_loc (BaseType IntType) (BaseType (type_of_rt_val v))
  | (Eq | Neq | Lt | Gt | Leq | Geq), RTDouble _, v ->
      type_error ~loc:e2_loc (BaseType DoubleType) (BaseType (type_of_rt_val v))
  | (Eq | Neq | Lt | Gt | Leq | Geq), v, _ ->
      type_error ~loc:e1_loc (BaseType IntType) (BaseType (type_of_rt_val v))

let eval_un_op ~e_loc op v =
  match (op, v) with
  | Neg, RTInt i -> WRM.ok (RTInt (-i))
  | Not, RTBool b -> WRM.ok (RTBool (not b))
  | Neg, v ->
      type_error ~loc:e_loc (BaseType IntType) (BaseType (type_of_rt_val v))
  | Not, v ->
      type_error ~loc:e_loc (BaseType BoolType) (BaseType (type_of_rt_val v))

let eval_base_val Loc.{ it = v } =
  match v with
  | IntVal i -> RTInt i
  | BoolVal b -> RTBool b
  | DoubleVal d -> RTDouble d
  | CharVal c -> RTChar c

let rec eval_expr env mem Loc.{ it = e; at = loc } =
  match e with
  | Val v -> WRM.ok (eval_base_val v, mem)
  | Var id ->
      let* v = lookup_var ~loc env mem id in
      WRM.ok (v, mem)
  | BinOp (e1, op, e2) ->
      let* v1, mem = eval_expr env mem e1 in
      let* v2, mem = eval_expr env mem e2 in
      let* v = eval_bin_op ~e1_loc:e1.at ~e2_loc:e2.at op v1 v2 in
      WRM.ok (v, mem)
  | UnOp (op, e) ->
      let* v, mem = eval_expr env mem e in
      let* v = eval_un_op ~e_loc:e.at op v in
      WRM.ok (v, mem)
  | ArrayLoad (id, e) ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_int arr_ptr (expected_array_error ~loc id) in
      let* idx, mem = eval_expr env mem e in
      let* idx = cast_int idx (type_error ~loc:e.at (BaseType IntType)) in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error ~loc id) in
      if idx < 0 || idx >= arr_len then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let* datum = lookup_addr ~loc mem (arr_ptr + idx + 1) in
        WRM.ok (datum, mem)
  | NewArrayDefault (t, e) ->
      let* n, mem = eval_expr env mem e in
      let* n = cast_int n (type_error ~loc:e.at (BaseType IntType)) in
      if n <= 0 then error_with_loc ~loc:e.at "array size must be positive"
      else
        let data = List.init n (fun _ -> default_value_of_type t) in
        WRM.ok (alloc_array data mem)
  | NewArrayLiteral vs -> WRM.ok (alloc_array (List.map eval_base_val vs) mem)
  | ArrayLength id ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_int arr_ptr (expected_array_error ~loc id) in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error ~loc id) in
      WRM.ok (RTInt arr_len, mem)
  | Cast (t, e) ->
      let* v, mem = eval_expr env mem e in
      let* v =
        match (t, v) with
        | BaseType IntType, RTDouble d -> WRM.ok (RTInt (int_of_float d))
        | BaseType DoubleType, RTInt i -> WRM.ok (RTDouble (float_of_int i))
        | t, v when BaseType (type_of_rt_val v) = t -> WRM.ok v
        | t, v -> cast_error ~loc t (BaseType (type_of_rt_val v))
      in
      WRM.ok (v, mem)
  | Call (id, args) ->
      let* params, body = lookup_fun ~loc env id in
      let decls = Declare (params, Some args) in
      let* env, mem = eval_command env mem Loc.{ it = decls; at = loc } in
      eval_expr env mem body
  | ExprBlock (c, e) ->
      let* env, mem = eval_command env mem c in
      eval_expr env mem e
  | Ternary (e1, e2, e3) ->
      let* v1, mem = eval_expr env mem e1 in
      let* v1 = cast_bool v1 (type_error ~loc:e1.at (BaseType BoolType)) in
      if v1 then eval_expr env mem e2 else eval_expr env mem e3

and eval_command env mem Loc.{ it = c; at = loc } =
  match c with
  | Skip -> WRM.ok (env, mem)
  | Declare (ds, None) ->
      WRM.foldM
        (fun (env, mem) (t, id) ->
          match t with
          | BaseType t ->
              let mem, addr = Mem.alloc (default_value_of_type t) mem in
              let env = Env.add id (Addr addr) env in
              WRM.ok (env, mem)
          | CompositeType _ ->
              unreachable
                "composite types should not appear in declarations with \
                 default values"
          | FuncType _ ->
              unreachable "function types should not appear in declarations")
        (env, mem) ds
  | Declare (ds, Some es) ->
      WRM.foldM
        (fun (env, mem) ((t, id), e) ->
          let* v, mem = eval_expr env mem e in
          match t with
          | BaseType _ | CompositeType _ ->
              let mem, addr = Mem.alloc v mem in
              let env = Env.add id (Addr addr) env in
              WRM.ok (env, mem)
          | FuncType _ ->
              unreachable "function types should not appear in declarations")
        (env, mem) (List.combine ds es)
  | Assign (ids, es) ->
      WRM.foldM
        (fun (env, mem) (id, e) ->
          let* v, mem = eval_expr env mem e in
          let* mem = store_var ~loc env mem id v in
          WRM.ok (env, mem))
        (env, mem) (List.combine ids es)
  | ArrayStore (id, e1, e2) ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_int arr_ptr (expected_array_error ~loc id) in
      let* idx, mem = eval_expr env mem e1 in
      let* idx = cast_int idx (type_error ~loc:e1.at (BaseType IntType)) in
      let* datum, mem = eval_expr env mem e2 in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error ~loc id) in
      if idx < 0 || idx >= arr_len then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let mem = Mem.add (arr_ptr + idx + 1) datum mem in
        WRM.ok (env, mem)
  | Seq (c1, c2) ->
      let* env, mem = eval_command env mem c1 in
      eval_command env mem c2
  | IfThenElse (e, c1, c2) ->
      let* v, mem = eval_expr env mem e in
      let* v = cast_bool v (type_error ~loc:e.at (BaseType BoolType)) in
      let* _, mem =
        if v then eval_command env mem c1 else eval_command env mem c2
      in
      WRM.ok (env, mem)
  | While (e, c) ->
      let* v, mem = eval_expr env mem e in
      let* v = cast_bool v (type_error ~loc:e.at (BaseType BoolType)) in
      if v then
        let* _, mem = eval_command env mem c in
        eval_command env mem Loc.{ it = While (e, c); at = loc }
      else WRM.ok (env, mem)
  | Block c ->
      let* _, mem = eval_command env mem c in
      WRM.ok (env, mem)
  | Proc (id, ds, c) ->
      let env = Env.add id (LambdaProc (ds, c)) env in
      WRM.ok (env, mem)
  | Invoke (id, args) ->
      let* params, body = lookup_proc ~loc env id in
      let decls = Declare (params, Some args) in
      let* env', mem = eval_command env mem Loc.{ it = decls; at = loc } in
      let* _, mem = eval_command env' mem body in
      WRM.ok (env, mem)
  | Func (id, ds, t, e) ->
      let env = Env.add id (LambdaFun (ds, e)) env in
      WRM.ok (env, mem)

let run_program c = eval_command Env.empty Mem.empty c
