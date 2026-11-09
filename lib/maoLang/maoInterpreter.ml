open MaoAst
open MaoAst_pp
open Utils
open WriterResultMonad.Syntax

module IntAddr = struct
  type t = int

  let zero = 0
  let next x = x + 1
  let compare = compare
  let pp fmt = Fmt.pf fmt "0x%x"
end

module Mem = Mem.Make (IntAddr)
module WRM = WriterResultMonad

type idk_what_to_call_it =
  | Addr of IntAddr.t
  | LambdaFun of typed_var list * checked_expr
  | LambdaProc of typed_var list * checked_command

module RTVal = struct
  type t =
    | Int of int
    | Bool of bool
    | Double of float
    | Char of char
    | Addr of IntAddr.t

  let type_of = function
    | Int _ -> BaseType IntType
    | Bool _ -> BaseType BoolType
    | Double _ -> BaseType DoubleType
    | Char _ -> BaseType CharType
    | Addr _ -> unreachable "cannot determine type of address value"

  let default_from_type = function
    | IntType -> Int 0
    | BoolType -> Bool false
    | DoubleType -> Double 0.0
    | CharType -> Char '\000'

  let pp fmt = function
    | Int i -> Fmt.pf fmt "%d" i
    | Bool b -> Fmt.pf fmt "%b" b
    | Double d -> Fmt.string fmt (shortest_float_string d)
    | Char c -> Fmt.pf fmt "'%c'" c
    | Addr addr -> IntAddr.pp fmt addr
end

type env = idk_what_to_call_it Env.t
type mem = RTVal.t Mem.t
type log_kind = CmdLog | ExprLog
type log = { env : env; mem : mem; event : string; descr : string }

let log ~loc env mem event fmt =
  Fmt.kstr
    (fun descr -> { env; mem; event; descr } |> Loc.with_loc ~loc |> WRM.tell)
    fmt

let undefined_variable_error id = unreachable "undefined variable '%s'" id

let type_error expected got =
  unreachable "expected %a but got %a" pp_mao_type expected pp_mao_type got

let expected_array_error id t =
  unreachable "expected '%s' to be an array but got %a" id pp_mao_type t

let cast_error t1 t2 =
  unreachable "cannot cast from %a to %a" pp_mao_type t2 pp_mao_type t1

let expected_address_error id =
  unreachable "expected '%s' to have a memory address" id

let expected_function_error id = unreachable "expected '%s' to be a function" id

let expected_procedure_error id =
  unreachable "expected '%s' to be a procedure" id

let undefined_memory_error addr =
  unreachable "undefined memory at address '%a'" IntAddr.pp addr

let error_with_loc ~loc fmt = Fmt.kstr (Loc.with_loc ~loc >> WRM.error) fmt
let division_by_zero_error ~loc = error_with_loc ~loc "division by zero"

let index_out_of_bounds_error ~loc idx arr_len =
  error_with_loc ~loc "index %d out of bounds for array of length %d" idx
    arr_len

let cast_int v cont =
  match v with RTVal.Int i -> WRM.ok i | v -> cont RTVal.(type_of v)

let cast_addr v cont =
  match v with RTVal.Addr addr -> WRM.ok addr | v -> cont RTVal.(type_of v)

let cast_bool v cont =
  match v with RTVal.Bool b -> WRM.ok b | v -> cont RTVal.(type_of v)

let lookup_addr ~loc mem addr =
  match Mem.find_opt addr mem with
  | Some v -> WRM.ok v
  | None -> undefined_memory_error addr

let lookup_var ~loc env mem id =
  match Env.find_opt id env with
  | Some (Addr addr) -> lookup_addr ~loc mem addr
  | Some (LambdaFun _) | Some (LambdaProc _) -> expected_address_error id
  | None -> undefined_variable_error id

let store_var ~loc env mem id v =
  match Env.find_opt id env with
  | Some (Addr addr) -> WRM.ok (Mem.add addr v mem)
  | Some (LambdaFun _) | Some (LambdaProc _) -> expected_address_error id
  | None -> undefined_variable_error id

let lookup_fun ~loc env id =
  match Env.find_opt id env with
  | Some (LambdaFun (params, body)) -> WRM.ok (params, body)
  | Some (Addr _) | Some (LambdaProc _) -> expected_function_error id
  | None -> undefined_variable_error id

let lookup_proc ~loc env id =
  match Env.find_opt id env with
  | Some (LambdaProc (params, body)) -> WRM.ok (params, body)
  | Some (Addr _) | Some (LambdaFun _) -> expected_procedure_error id
  | None -> undefined_variable_error id

let alloc_array data mem =
  let n = List.length data in
  let mem, arr_ptr = Mem.alloc (RTVal.Int n) mem in
  let addrs = List.init n (fun i -> arr_ptr + i + 1) in
  let mem =
    List.fold_left2
      (fun mem addr datum -> Mem.add addr datum mem)
      mem addrs data
  in
  (RTVal.Addr arr_ptr, mem)

let eval_bin_op ~e1_loc ~e2_loc op v1 v2 =
  let module V = RTVal in
  match (op, v1, v2) with
  | Add, V.Int i1, V.Int i2 -> WRM.ok (V.Int (i1 + i2))
  | Sub, V.Int i1, V.Int i2 -> WRM.ok (V.Int (i1 - i2))
  | Mul, V.Int i1, V.Int i2 -> WRM.ok (V.Int (i1 * i2))
  | Div, V.Int i1, V.Int i2 ->
      if i2 = 0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (V.Int (i1 / i2))
  | Mod, V.Int i1, V.Int i2 ->
      if i2 = 0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (V.Int (i1 mod i2))
  | Add, V.Double d1, V.Double d2 -> WRM.ok (V.Double (d1 +. d2))
  | Sub, V.Double d1, V.Double d2 -> WRM.ok (V.Double (d1 -. d2))
  | Mul, V.Double d1, V.Double d2 -> WRM.ok (V.Double (d1 *. d2))
  | Div, V.Double d1, V.Double d2 ->
      if d2 = 0.0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (V.Double (d1 /. d2))
  | Mod, V.Double d1, V.Double d2 ->
      if d2 = 0.0 then division_by_zero_error ~loc:e2_loc
      else WRM.ok (V.Double (mod_float d1 d2))
  | (Add | Sub | Mul | Div | Mod), V.Int _, v ->
      type_error (BaseType IntType) V.(type_of v)
  | (Add | Sub | Mul | Div | Mod), V.Double _, v ->
      type_error (BaseType DoubleType) V.(type_of v)
  | (Add | Sub | Mul | Div | Mod), v, _ ->
      type_error (BaseType IntType) V.(type_of v)
  | And, V.Bool b1, V.Bool b2 -> WRM.ok (V.Bool (b1 && b2))
  | Or, V.Bool b1, V.Bool b2 -> WRM.ok (V.Bool (b1 || b2))
  | Xor, V.Bool b1, V.Bool b2 -> WRM.ok (V.Bool (b1 <> b2))
  | (And | Or | Xor), V.Bool _, v ->
      type_error (BaseType BoolType) V.(type_of v)
  | (And | Or | Xor), _, v -> type_error (BaseType BoolType) V.(type_of v)
  | Eq, V.Int i1, V.Int i2 -> WRM.ok (V.Bool (i1 = i2))
  | Neq, V.Int i1, V.Int i2 -> WRM.ok (V.Bool (i1 <> i2))
  | Lt, V.Int i1, V.Int i2 -> WRM.ok (V.Bool (i1 < i2))
  | Gt, V.Int i1, V.Int i2 -> WRM.ok (V.Bool (i1 > i2))
  | Leq, V.Int i1, V.Int i2 -> WRM.ok (V.Bool (i1 <= i2))
  | Geq, V.Int i1, V.Int i2 -> WRM.ok (V.Bool (i1 >= i2))
  | Eq, V.Double d1, V.Double d2 -> WRM.ok (V.Bool (d1 = d2))
  | Neq, V.Double d1, V.Double d2 -> WRM.ok (V.Bool (d1 <> d2))
  | Lt, V.Double d1, V.Double d2 -> WRM.ok (V.Bool (d1 < d2))
  | Gt, V.Double d1, V.Double d2 -> WRM.ok (V.Bool (d1 > d2))
  | Leq, V.Double d1, V.Double d2 -> WRM.ok (V.Bool (d1 <= d2))
  | Geq, V.Double d1, V.Double d2 -> WRM.ok (V.Bool (d1 >= d2))
  | (Eq | Neq | Lt | Gt | Leq | Geq), V.Int _, v ->
      type_error (BaseType IntType) V.(type_of v)
  | (Eq | Neq | Lt | Gt | Leq | Geq), V.Double _, v ->
      type_error (BaseType DoubleType) V.(type_of v)
  | (Eq | Neq | Lt | Gt | Leq | Geq), v, _ ->
      type_error (BaseType IntType) V.(type_of v)

let eval_un_op ~e_loc op v =
  match (op, v) with
  | Neg, RTVal.Int i -> WRM.ok (RTVal.Int (-i))
  | Not, RTVal.Bool b -> WRM.ok (RTVal.Bool (not b))
  | Neg, v -> type_error (BaseType IntType) RTVal.(type_of v)
  | Not, v -> type_error (BaseType BoolType) RTVal.(type_of v)

let eval_base_val Loc.{ it = v } =
  match v with
  | IntVal i -> RTVal.Int i
  | BoolVal b -> RTVal.Bool b
  | DoubleVal d -> RTVal.Double d
  | CharVal c -> RTVal.Char c

let eval_decl ~loc env mem (t, id) v =
  let mem, addr = Mem.alloc v mem in
  let env = Env.add id (Addr addr) env in
  WRM.ok (env, mem)

let rec eval_expr env mem Loc.{ it = e; at = loc } =
  match e with
  | CVal v -> WRM.ok (eval_base_val v, mem)
  | CVar id ->
      let* v = lookup_var ~loc env mem id in
      WRM.ok (v, mem)
  | CBinOp (e1, op, e2) ->
      let* v1, mem = eval_expr env mem e1 in
      let* v2, mem = eval_expr env mem e2 in
      let* v = eval_bin_op ~e1_loc:e1.at ~e2_loc:e2.at op v1 v2 in
      WRM.ok (v, mem)
  | CUnOp (op, e) ->
      let* v, mem = eval_expr env mem e in
      let* v = eval_un_op ~e_loc:e.at op v in
      WRM.ok (v, mem)
  | CArrayLoad (id, e) ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_addr arr_ptr (expected_array_error id) in
      let* idx, mem = eval_expr env mem e in
      let* idx = cast_int idx (type_error (BaseType IntType)) in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error id) in
      if idx < 0 || idx >= arr_len then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let* v = lookup_addr ~loc mem (arr_ptr + idx + 1) in
        WRM.ok (v, mem)
  | CNewArrayDefault (t, e) ->
      let* n, mem = eval_expr env mem e in
      let* n = cast_int n (type_error (BaseType IntType)) in
      if n <= 0 then error_with_loc ~loc:e.at "array size must be positive"
      else
        let data = List.init n (fun _ -> RTVal.default_from_type t) in
        let arr_ptr, mem = alloc_array data mem in
        WRM.ok (arr_ptr, mem)
  | CNewArrayLiteral vs ->
      let vs = List.map eval_base_val vs in
      let arr_ptr, mem = alloc_array vs mem in
      WRM.ok (arr_ptr, mem)
  | CArrayLength id ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_addr arr_ptr (expected_array_error id) in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error id) in
      WRM.ok (RTVal.Int arr_len, mem)
  | CCast (t, e) ->
      let* v, mem = eval_expr env mem e in
      let* v =
        match (t, v) with
        | BaseType IntType, RTVal.Double d ->
            WRM.ok (RTVal.Int (int_of_float d))
        | BaseType DoubleType, RTVal.Int i ->
            WRM.ok (RTVal.Double (float_of_int i))
        | _ -> cast_error t RTVal.(type_of v)
      in
      WRM.ok (v, mem)
  | CExprBlock (c, e) ->
      let* env, mem = eval_command env mem c in
      eval_expr env mem e
  | CCall (id, args) ->
      let* params, body = lookup_fun ~loc env id in
      let decls = Loc.with_loc ~loc (CDeclare (List.combine params args)) in
      let e = Loc.with_loc ~loc (CExprBlock (decls, body)) in
      let* () = log ~loc env mem "begin call" "%s(...)" id in
      let* v, mem = eval_expr env mem e in
      let* () = log ~loc env mem "end call" "%s(...) -> %a" id RTVal.pp v in
      WRM.ok (v, mem)
  | CTernary (e1, e2, e3) ->
      let* v1, mem = eval_expr env mem e1 in
      let* b = cast_bool v1 (type_error (BaseType BoolType)) in
      if b then eval_expr env mem e2 else eval_expr env mem e3

and eval_command env mem (Loc.{ it = c; at = loc } as c_loc) =
  match c with
  | CSkip -> WRM.ok (env, mem)
  | CDeclare [ ((t, id), e) ] ->
      let* v, mem = eval_expr env mem e in
      let* () =
        log ~loc env mem "decl" "%a %s = %a" pp_mao_type t id RTVal.pp v
      in
      eval_decl ~loc env mem (t, id) v
  | CDeclare (((t, id), e) :: rest) ->
      let* v, mem = eval_expr env mem e in
      let rest = Loc.with_loc ~loc (CDeclare rest) in
      let* env, mem = eval_command env mem rest in
      let* env, mem = eval_decl ~loc:e.at env mem (t, id) v in
      let* () =
        log ~loc env mem "decls" "%a %s = %a" pp_mao_type t id RTVal.pp v
      in
      WRM.ok (env, mem)
  | CDeclare [] -> unreachable "declaration with no variables and no values"
  | CAssign [ (id, e) ] ->
      let* v, mem = eval_expr env mem e in
      let* mem = store_var ~loc env mem id v in
      let* () = log ~loc env mem "assign" "%s := %a" id RTVal.pp v in
      WRM.ok (env, mem)
  | CAssign ((id, e) :: res) ->
      let* v, mem = eval_expr env mem e in
      let rest = Loc.with_loc ~loc (CAssign res) in
      let* env, mem = eval_command env mem rest in
      let* mem = store_var ~loc env mem id v in
      let* () = log ~loc env mem "assigns" "%s := %a" id RTVal.pp v in
      WRM.ok (env, mem)
  | CAssign [] -> unreachable "assignment with no variables"
  | CArrayStore (id, e1, e2) ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_addr arr_ptr (expected_array_error id) in
      let* idx, mem = eval_expr env mem e1 in
      let* idx = cast_int idx (type_error (BaseType IntType)) in
      let* v, mem = eval_expr env mem e2 in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error id) in
      if idx < 0 || idx >= arr_len then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let addr = arr_ptr + idx + 1 in
        let mem = Mem.add addr v mem in
        let* () =
          log ~loc env mem "store" "%s[%d] (mem[%a]) := %a" id idx RTVal.pp
            (RTVal.Addr addr) RTVal.pp v
        in
        WRM.ok (env, mem)
  | CSeq (c1, c2) ->
      let* env, mem = eval_command env mem c1 in
      eval_command env mem c2
  | CIfThenElse (e, c1, c2) ->
      let* v, mem = eval_expr env mem e in
      let* b = cast_bool v (type_error (BaseType BoolType)) in
      let* () = log ~loc:e.at env mem "if" "guard is %b" b in
      let* _, mem =
        if b then eval_command env mem c1 else eval_command env mem c2
      in
      WRM.ok (env, mem)
  | CWhile (e, c) ->
      let* v, mem = eval_expr env mem e in
      let* v = cast_bool v (type_error (BaseType BoolType)) in
      let* () = log ~loc:e.at env mem "while" "guard is %b" v in
      if v then
        let* _, mem = eval_command env mem c in
        eval_command env mem c_loc
      else WRM.ok (env, mem)
  | CBlock c ->
      let* _, mem = eval_command env mem c in
      WRM.ok (env, mem)
  | CProc (id, ds, c) ->
      let env = Env.add id (LambdaProc (ds, c)) env in
      WRM.ok (env, mem)
  | CInvoke (id, args) ->
      let* params, body = lookup_proc ~loc env id in
      let decls = Loc.with_loc ~loc (CDeclare (List.combine params args)) in
      let body = Loc.with_loc ~loc (CSeq (decls, body)) in
      let body = Loc.with_loc ~loc (CBlock body) in
      let* () = log ~loc env mem "begin invoke" "%s(...)" id in
      let* _, mem = eval_command env mem body in
      let* () = log ~loc env mem "end invoke" "%s(...)" id in
      WRM.ok (env, mem)
  | CFunc (id, ds, t, e) ->
      let env = Env.add id (LambdaFun (ds, e)) env in
      WRM.ok (env, mem)

let run_program c = eval_command Env.empty Mem.empty c
