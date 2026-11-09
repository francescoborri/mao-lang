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

module RTVal = struct
  type t = Int of int | Bool of bool | Double of float | Char of char

  let type_of = function
    | Int _ -> BaseType IntType
    | Bool _ -> BaseType BoolType
    | Double _ -> BaseType DoubleType
    | Char _ -> BaseType CharType

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
end

type env = idk_what_to_call_it Env.t
type mem = RTVal.t Mem.t
type log = { event : string; env : env; mem : mem }

let log ~loc event env mem =
  { event; env; mem } |> Loc.with_loc ~loc |> WRM.tell

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
  (RTVal.Int arr_ptr, mem)

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

let eval_decl ~loc env mem ?(v = None) (t, id) =
  match (t, v) with
  | BaseType _, Some v | CompositeType _, Some v ->
      let* () = log ~loc (Fmt.str "let %s = %a" id RTVal.pp v) env mem in
      let mem, addr = Mem.alloc v mem in
      let env = Env.add id (Addr addr) env in
      WRM.ok (env, mem)
  | BaseType t, None ->
      let v = RTVal.default_from_type t in
      let* () = log ~loc (Fmt.str "let %s = %a" id RTVal.pp v) env mem in
      let mem, addr = Mem.alloc v mem in
      let env = Env.add id (Addr addr) env in
      WRM.ok (env, mem)
  | CompositeType (Array _), None ->
      unreachable
        "array type should not appear in declarations with default values"
  | FuncType _, _ ->
      unreachable "function types should not appear in declarations"

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
      let* arr_ptr = cast_int arr_ptr (expected_array_error id) in
      let* idx, mem = eval_expr env mem e in
      let* idx = cast_int idx (type_error (BaseType IntType)) in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error id) in
      if idx < 0 || idx >= arr_len then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let* datum = lookup_addr ~loc mem (arr_ptr + idx + 1) in
        WRM.ok (datum, mem)
  | NewArrayDefault (t, e) ->
      let* n, mem = eval_expr env mem e in
      let* n = cast_int n (type_error (BaseType IntType)) in
      if n <= 0 then error_with_loc ~loc:e.at "array size must be positive"
      else
        let data = List.init n (fun _ -> RTVal.default_from_type t) in
        WRM.ok (alloc_array data mem)
  | NewArrayLiteral vs -> WRM.ok (alloc_array (List.map eval_base_val vs) mem)
  | ArrayLength id ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_int arr_ptr (expected_array_error id) in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error id) in
      WRM.ok (RTVal.Int arr_len, mem)
  | Cast (t, e) ->
      let* v, mem = eval_expr env mem e in
      let* v =
        match (t, v) with
        | BaseType IntType, RTVal.Double d ->
            WRM.ok (RTVal.Int (int_of_float d))
        | BaseType DoubleType, RTVal.Int i ->
            WRM.ok (RTVal.Double (float_of_int i))
        | t, v -> cast_error t RTVal.(type_of v)
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
      let* v1 = cast_bool v1 (type_error (BaseType BoolType)) in
      if v1 then eval_expr env mem e2 else eval_expr env mem e3

and eval_command env mem Loc.{ it = c; at = loc } =
  match c with
  | Skip ->
      let* () = log ~loc "skip" env mem in
      WRM.ok (env, mem)
  | Declare ([ (t, id) ], None) -> eval_decl ~loc env mem (t, id)
  | Declare ((t, id) :: ds, None) ->
      let* env, mem =
        eval_command env mem Loc.{ it = Declare (ds, None); at = loc }
      in
      eval_decl ~loc env mem (t, id)
  | Declare ([ (t, id) ], Some [ e ]) ->
      let* v, mem = eval_expr env mem e in
      eval_decl ~loc env mem ~v:(Some v) (t, id)
  | Declare ((t, id) :: ds, Some (e :: es)) ->
      let* v, mem = eval_expr env mem e in
      let* env, mem =
        eval_command env mem Loc.{ it = Declare (ds, Some es); at = loc }
      in
      let* env, mem = eval_decl ~loc:e.at env mem ~v:(Some v) (t, id) in
      WRM.ok (env, mem)
  | Declare ([], None) ->
      unreachable "default declaration with no variables and no values"
  | Declare ([], Some []) ->
      unreachable "declaration with no variables and no values"
  | Declare ([], Some (_ :: _)) | Declare (_ :: _, Some []) ->
      unreachable "declaration with mismatched number of variables and values"
  | Assign ([ id ], [ e ]) ->
      let* v, mem = eval_expr env mem e in
      let* () = log ~loc:e.at (Fmt.str "%s := %a" id RTVal.pp v) env mem in
      let* mem = store_var ~loc env mem id v in
      WRM.ok (env, mem)
  | Assign (id :: ids, e :: es) ->
      let* v, mem = eval_expr env mem e in
      let* env, mem =
        eval_command env mem Loc.{ it = Assign (ids, es); at = loc }
      in
      let* () = log ~loc:e.at (Fmt.str "%s := %a" id RTVal.pp v) env mem in
      let* mem = store_var ~loc env mem id v in
      WRM.ok (env, mem)
  | Assign ([], []) -> unreachable "assignment with no variables"
  | Assign ([], _ :: _) | Assign (_ :: _, []) ->
      unreachable "assignment with mismatched number of variables and values"
  | ArrayStore (id, e1, e2) ->
      let* arr_ptr = lookup_var ~loc env mem id in
      let* arr_ptr = cast_int arr_ptr (expected_array_error id) in
      let* idx, mem = eval_expr env mem e1 in
      let* idx = cast_int idx (type_error (BaseType IntType)) in
      let* datum, mem = eval_expr env mem e2 in
      let* arr_len = lookup_addr ~loc mem arr_ptr in
      let* arr_len = cast_int arr_len (expected_array_error id) in
      let* () =
        log ~loc (Fmt.str "%s[%d] := %a" id idx RTVal.pp datum) env mem
      in
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
      let* v = cast_bool v (type_error (BaseType BoolType)) in
      let* () = log ~loc (Fmt.str "if (%b)" v) env mem in
      let* _, mem =
        if v then eval_command env mem c1 else eval_command env mem c2
      in
      WRM.ok (env, mem)
  | While (e, c) ->
      let* v, mem = eval_expr env mem e in
      let* v = cast_bool v (type_error (BaseType BoolType)) in
      let* () = log ~loc (Fmt.str "while (%b)" v) env mem in
      if v then
        let* _, mem = eval_command env mem c in
        eval_command env mem Loc.{ it = While (e, c); at = loc }
      else WRM.ok (env, mem)
  | Block c ->
      let* _, mem = eval_command env mem c in
      WRM.ok (env, mem)
  | Proc (id, ds, c) ->
      let* () = log ~loc (Fmt.str "decl proc '%s'" id) env mem in
      let env = Env.add id (LambdaProc (ds, c)) env in
      WRM.ok (env, mem)
  | Invoke (id, args) ->
      let* params, body = lookup_proc ~loc env id in
      let decls = Declare (params, Some args) in
      let* () = log ~loc (Fmt.str "invoke '%s'" id) env mem in
      let* env', mem = eval_command env mem Loc.{ it = decls; at = loc } in
      let* _, mem = eval_command env' mem body in
      WRM.ok (env, mem)
  | Func (id, ds, t, e) ->
      let* () = log ~loc (Fmt.str "decl func '%s'" id) env mem in
      let env = Env.add id (LambdaFun (ds, e)) env in
      WRM.ok (env, mem)

let run_program c = eval_command Env.empty Mem.empty c
