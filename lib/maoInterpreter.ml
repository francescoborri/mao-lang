(*open MaoAst
open MaoAst_pp
open Utils
open ErrorUtils
open LoggerUtils
open LoggerUtils.Syntax

type rt_val =
  | RTInt of int
  | RTBool of bool
  | RTDouble of float
  | RTChar of char

type env = Addr.t Env.t
type mem = rt_val AddrMap.t
type log = { event : string; loc : Loc.loc; expr : expr_node option }

let default_value_of_type = function
  | IntType -> RTInt 0
  | BoolType -> RTBool false
  | DoubleType -> RTDouble 0.0
  | CharType -> RTChar '\000'

let eval_bin_op ~e1_loc ~e2_loc op v1 v2 =
  match (op, v1, v2) with
  | Add, RTInt i1, RTInt i2 -> Ok (RTInt (i1 + i2))
  | Sub, RTInt i1, RTInt i2 -> Ok (RTInt (i1 - i2))
  | Mul, RTInt i1, RTInt i2 -> Ok (RTInt (i1 * i2))
  | Div, RTInt i1, RTInt i2 ->
      if i2 = 0 then located_error ~loc:e2_loc "Division by zero"
      else Ok (RTInt (i1 / i2))
  | Mod, RTInt i1, RTInt i2 ->
      if i2 = 0 then located_error ~loc:e2_loc "Modulo by zero"
      else Ok (RTInt (i1 mod i2))
  | Add, RTDouble d1, RTDouble d2 -> Ok (RTDouble (d1 +. d2))
  | Sub, RTDouble d1, RTDouble d2 -> Ok (RTDouble (d1 -. d2))
  | Mul, RTDouble d1, RTDouble d2 -> Ok (RTDouble (d1 *. d2))
  | Div, RTDouble d1, RTDouble d2 ->
      if d2 = 0.0 then located_error ~loc:e2_loc "Division by zero"
      else Ok (RTDouble (d1 /. d2))
  | Mod, RTDouble d1, RTDouble d2 ->
      if d2 = 0.0 then located_error ~loc:e2_loc "Modulo by zero"
      else Ok (RTDouble (mod_float d1 d2))
  | (Add | Sub | Mul | Div | Mod), RTInt _, _ ->
      located_error ~loc:e2_loc "Expected an int"
  | (Add | Sub | Mul | Div | Mod), RTDouble _, _ ->
      located_error ~loc:e2_loc "Expected a double"
  | (Add | Sub | Mul | Div | Mod), _, _ ->
      located_error ~loc:e1_loc "Expected an int"
  | And, RTBool b1, RTBool b2 -> Ok (RTBool (b1 && b2))
  | Or, RTBool b1, RTBool b2 -> Ok (RTBool (b1 || b2))
  | Xor, RTBool b1, RTBool b2 -> Ok (RTBool (b1 <> b2))
  | (And | Or | Xor), RTBool _, _ -> located_error ~loc:e2_loc "Expected a bool"
  | (And | Or | Xor), _, _ -> located_error ~loc:e1_loc "Expected a bool"
  | Eq, RTInt i1, RTInt i2 -> Ok (RTBool (i1 = i2))
  | Neq, RTInt i1, RTInt i2 -> Ok (RTBool (i1 <> i2))
  | Lt, RTInt i1, RTInt i2 -> Ok (RTBool (i1 < i2))
  | Gt, RTInt i1, RTInt i2 -> Ok (RTBool (i1 > i2))
  | Leq, RTInt i1, RTInt i2 -> Ok (RTBool (i1 <= i2))
  | Geq, RTInt i1, RTInt i2 -> Ok (RTBool (i1 >= i2))
  | Eq, RTDouble d1, RTDouble d2 -> Ok (RTBool (d1 = d2))
  | Neq, RTDouble d1, RTDouble d2 -> Ok (RTBool (d1 <> d2))
  | Lt, RTDouble d1, RTDouble d2 -> Ok (RTBool (d1 < d2))
  | Gt, RTDouble d1, RTDouble d2 -> Ok (RTBool (d1 > d2))
  | Leq, RTDouble d1, RTDouble d2 -> Ok (RTBool (d1 <= d2))
  | Geq, RTDouble d1, RTDouble d2 -> Ok (RTBool (d1 >= d2))
  | (Eq | Neq | Lt | Gt | Leq | Geq), RTInt _, _ ->
      located_error ~loc:e2_loc "Expected an int"
  | (Eq | Neq | Lt | Gt | Leq | Geq), RTDouble _, _ ->
      located_error ~loc:e2_loc "Expected a double"
  | (Eq | Neq | Lt | Gt | Leq | Geq), _, _ ->
      located_error ~loc:e1_loc "Expected an int"

let eval_un_op ~e_loc op v =
  match (op, v) with
  | Neg, RTInt i -> Ok (RTInt (-i))
  | Not, RTBool b -> Ok (RTBool (not b))
  | Neg, _ -> located_error ~loc:e_loc "Expected an int" @@ fail
  | Not, _ -> located_error ~loc:e_loc "Expected a bool" @@ fail

let rec eval_expr ~env ~mem { it = e; at = loc } =
  match e with
  | Val { it = IntVal i } -> Ok (RTInt i, mem)
  | Val { it = BoolVal b } -> Ok (RTBool b, mem)
  | Val { it = DoubleVal d } -> Ok (RTDouble d, mem)
  | Val { it = CharVal c } -> Ok (RTChar c, mem)
  | Var id -> (
      let* addr =
        match Env.find_opt id env with
        | None -> undefined_variable_error ~loc id
        | Some addr -> Ok addr
      in
      match AddrMap.find_opt addr mem with
      | Some v -> Ok (v, mem)
      | None -> located_error ~loc "Address %a not found in memory" Addr.pp addr
      )
  | BinOp (e1, op, e2) ->
      let* v1, mem = eval_expr ~env ~mem e1 in
      let* v2, mem = eval_expr ~env ~mem e2 in
      let* v = eval_bin_op ~e1_loc:e1.at ~e2_loc:e2.at op v1 v2 in
      Ok (v, mem)
  | UnOp (op, e) ->
      let* v, mem = eval_expr ~env ~mem e in
      let* v = eval_un_op ~e_loc:e.at op v in
      Ok (v, mem)
  | ArrayLoad (id, e) -> (
      let* id_addr =
        match Env.find_opt id env with
        | None -> undefined_variable_error ~loc id
        | Some addr -> Ok addr
      in
      let* arr_ptr =
        match AddrMap.find_opt id_addr mem with
        | None ->
            located_error ~loc "Address %a not found in memory" Addr.pp id_addr
        | Some (RTBool _) | Some (RTDouble _) | Some (RTChar _) ->
            located_error ~loc "Variable %s is not an array" id
        | Some (RTInt addr) -> Ok (Addr.of_int addr)
      in
      let* idx, mem = eval_expr ~env ~mem e in
      let* idx =
        match idx with
        | RTInt idx -> Ok idx
        | _ -> located_error ~loc:e.at "Expected an int"
      in
      match AddrMap.find_opt arr_ptr mem with
      | None ->
          located_error ~loc "Address %a not found in memory" Addr.pp arr_ptr
      | Some (RTBool _) | Some (RTDouble _) | Some (RTChar _) ->
          located_error ~loc "Address %a is not an array" Addr.pp arr_ptr
      | Some (RTInt arr_len) -> (
          if idx < 0 || idx >= arr_len then
            located_error ~loc:e.at
              "Index %d out of bounds for array of length %d" idx arr_len
          else
            let addr = Addr.add arr_ptr (idx + 1) in
            match AddrMap.find_opt addr mem with
            | Some v -> Ok (v, mem)
            | None ->
                located_error ~loc "Address %a not found in memory" Addr.pp addr
          ))
  | _ -> Error "Not implemented yet"
let eval_command ~env ~mem { it = c; at = loc } =
  match c with Skip -> Ok (env, mem) | _ -> Error "Not yet implemented"
*)
