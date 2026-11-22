open MaoAst
open MaoAst_pp
open MaoDataStructure
open ReaderStateResultMonad.Syntax
open Utils
module RSR = ReaderStateResultMonad

let undefined_variable_error id = unreachable "'%s' non è definito" id

let type_error t v =
  unreachable "atteso un valore di tipo %a, trovato %a" pp_mao_type t
    RTVal.pp_type v

let expected_array_error id v =
  unreachable "'%s' dovrebbe essere un array, ma contiene %a" id RTVal.pp_type v

let cast_error v t =
  unreachable "impossibile convertire da %a a %a" RTVal.pp_type v pp_mao_type t

let expected_address_error id =
  unreachable "'%s' dovrebbe avere un indirizzo di memoria" id

let expected_function_error id =
  unreachable "'%s' dovrebbe essere una funzione" id

let expected_procedure_error id =
  unreachable "'%s' dovrebbe essere una procedura" id

let undefined_memory_error addr =
  unreachable "memoria non definita all'indirizzo %a" Int64Addr.pp addr

let error_with_loc ~loc fmt = Fmt.kstr (Loc.with_loc ~loc >> RSR.error) fmt

type limits = { max_logs : int; max_depth : int }

let no_limits = { max_logs = max_int; max_depth = max_int }

let rsr_log ~loc state event =
  let* { max_logs } = RSR.ask () in
  let* n = RSR.get_log_id () in
  if n >= max_logs then
    error_with_loc ~loc "raggiunto il limite di %d passi" max_logs
  else { state; event } |> Loc.with_loc ~loc |> RSR.tell

let enter_call ~loc =
  let* { max_depth } = RSR.ask () in
  let* depth = RSR.enter_call () in
  if depth > max_depth then
    error_with_loc ~loc "superato il limite di %d chiamate annidate" max_depth
  else RSR.ok ()

let division_by_zero_error ~loc = error_with_loc ~loc "divisione per zero"

let cast_range_error ~loc v =
  error_with_loc ~loc "il valore %a non è rappresentabile come int" RTVal.pp v

let index_out_of_bounds_error ~loc idx arr_len =
  error_with_loc ~loc "indice %Ld fuori dai limiti di un array di lunghezza %Ld"
    idx arr_len

let set_env s env = { s with env }
let set_mem s mem = { s with mem }
let set_mem_snd s = Fun.id *** set_mem s
let replace_mem s { mem } = { s with mem }
let as_int ~expected = function RTVal.Int i -> i | v -> expected v
let as_addr ~expected = function RTVal.Addr addr -> addr | v -> expected v
let as_bool ~expected = function RTVal.Bool b -> b | v -> expected v

let lookup_addr addr mem =
  match Mem.get addr mem with
  | Some v -> v
  | None -> undefined_memory_error addr

let lookup_var id { env; mem } =
  match Env.find_opt id env with
  | Some (Addr addr) -> lookup_addr addr mem
  | Some (LambdaFun _) | Some (LambdaProc _) -> expected_address_error id
  | None -> undefined_variable_error id

let store_var id v { env; mem } =
  match Env.find_opt id env with
  | Some (Addr addr) -> Mem.set addr v mem
  | Some (LambdaFun _) | Some (LambdaProc _) -> expected_address_error id
  | None -> undefined_variable_error id

let lookup_fun id env =
  match Env.find_opt id env with
  | Some (LambdaFun (params, body)) -> (params, body)
  | Some (Addr _) | Some (LambdaProc _) -> expected_function_error id
  | None -> undefined_variable_error id

let lookup_proc id env =
  match Env.find_opt id env with
  | Some (LambdaProc (params, body)) -> (params, body)
  | Some (Addr _) | Some (LambdaFun _) -> expected_procedure_error id
  | None -> undefined_variable_error id

let alloc_array data mem =
  let n = data |> List.length |> Int64.of_int in
  let mem, arr_ptr = Mem.alloc (RTVal.Int n) mem in
  let mem = List.fold_left (fun mem v -> Mem.alloc v mem |> fst) mem data in
  (RTVal.Addr arr_ptr, mem)

let arr_len_addr arr_ptr = arr_ptr

let arr_elem_addr arr_ptr idx =
  arr_ptr |> Int64Addr.offset idx |> Int64Addr.next

let get_arr_ptr id =
  lookup_var id >> as_addr ~expected:(expected_array_error id)

let get_arr_len id arr_ptr =
  (fun { mem } -> mem)
  >> lookup_addr (arr_len_addr arr_ptr)
  >> as_int ~expected:(expected_array_error id)

let bind_params ~loc params args =
  let decls =
    if List.is_empty params then CSkip else CDeclare (List.combine params args)
  in
  Loc.with_loc ~loc decls

let eval_bin_op Loc.{ it = v1 } op Loc.{ it = v2; at = v2_loc } =
  let module V = RTVal in
  match (op, v1, v2) with
  | Add, V.Int i1, V.Int i2 -> RSR.ok (V.Int (Int64.add i1 i2))
  | Sub, V.Int i1, V.Int i2 -> RSR.ok (V.Int (Int64.sub i1 i2))
  | Mul, V.Int i1, V.Int i2 -> RSR.ok (V.Int (Int64.mul i1 i2))
  | Div, V.Int i1, V.Int i2 ->
      if Int64.equal i2 0L then division_by_zero_error ~loc:v2_loc
      else RSR.ok (V.Int (Int64.div i1 i2))
  | Mod, V.Int i1, V.Int i2 ->
      if Int64.equal i2 0L then division_by_zero_error ~loc:v2_loc
      else RSR.ok (V.Int (Int64.rem i1 i2))
  | Add, V.Double d1, V.Double d2 -> RSR.ok (V.Double (d1 +. d2))
  | Sub, V.Double d1, V.Double d2 -> RSR.ok (V.Double (d1 -. d2))
  | Mul, V.Double d1, V.Double d2 -> RSR.ok (V.Double (d1 *. d2))
  | Div, V.Double d1, V.Double d2 -> RSR.ok (V.Double (d1 /. d2))
  | Mod, V.Double d1, V.Double d2 -> RSR.ok (V.Double (mod_float d1 d2))
  | (Add | Sub | Mul | Div | Mod), V.Int _, v -> type_error (BaseType IntType) v
  | (Add | Sub | Mul | Div | Mod), V.Double _, v ->
      type_error (BaseType DoubleType) v
  | (Add | Sub | Mul | Div | Mod), v, _ -> type_error (BaseType IntType) v
  | And, V.Bool b1, V.Bool b2 -> RSR.ok (V.Bool (if b1 then b2 else false))
  | Or, V.Bool b1, V.Bool b2 -> RSR.ok (V.Bool (if b1 then true else b2))
  | Xor, V.Bool b1, V.Bool b2 -> RSR.ok (V.Bool (b1 <> b2))
  | (And | Or | Xor), V.Bool _, v -> type_error (BaseType BoolType) v
  | (And | Or | Xor), v, _ -> type_error (BaseType BoolType) v
  | Eq, V.Int i1, V.Int i2 -> RSR.ok (V.Bool (i1 = i2))
  | Neq, V.Int i1, V.Int i2 -> RSR.ok (V.Bool (i1 <> i2))
  | Lt, V.Int i1, V.Int i2 -> RSR.ok (V.Bool (i1 < i2))
  | Gt, V.Int i1, V.Int i2 -> RSR.ok (V.Bool (i1 > i2))
  | Leq, V.Int i1, V.Int i2 -> RSR.ok (V.Bool (i1 <= i2))
  | Geq, V.Int i1, V.Int i2 -> RSR.ok (V.Bool (i1 >= i2))
  | Eq, V.Double d1, V.Double d2 -> RSR.ok (V.Bool (d1 = d2))
  | Neq, V.Double d1, V.Double d2 -> RSR.ok (V.Bool (d1 <> d2))
  | Lt, V.Double d1, V.Double d2 -> RSR.ok (V.Bool (d1 < d2))
  | Gt, V.Double d1, V.Double d2 -> RSR.ok (V.Bool (d1 > d2))
  | Leq, V.Double d1, V.Double d2 -> RSR.ok (V.Bool (d1 <= d2))
  | Geq, V.Double d1, V.Double d2 -> RSR.ok (V.Bool (d1 >= d2))
  | Eq, V.Char c1, V.Char c2 -> RSR.ok (V.Bool (c1 = c2))
  | Neq, V.Char c1, V.Char c2 -> RSR.ok (V.Bool (c1 <> c2))
  | Lt, V.Char c1, V.Char c2 -> RSR.ok (V.Bool (c1 < c2))
  | Gt, V.Char c1, V.Char c2 -> RSR.ok (V.Bool (c1 > c2))
  | Leq, V.Char c1, V.Char c2 -> RSR.ok (V.Bool (c1 <= c2))
  | Geq, V.Char c1, V.Char c2 -> RSR.ok (V.Bool (c1 >= c2))
  | Eq, V.Bool b1, V.Bool b2 -> RSR.ok (V.Bool (b1 = b2))
  | Neq, V.Bool b1, V.Bool b2 -> RSR.ok (V.Bool (b1 <> b2))
  | (Eq | Neq | Lt | Gt | Leq | Geq), V.Int _, v ->
      type_error (BaseType IntType) v
  | (Eq | Neq | Lt | Gt | Leq | Geq), V.Double _, v ->
      type_error (BaseType DoubleType) v
  | (Eq | Neq | Lt | Gt | Leq | Geq), V.Char _, v ->
      type_error (BaseType CharType) v
  | (Eq | Neq), V.Bool _, v -> type_error (BaseType BoolType) v
  | (Eq | Neq | Lt | Gt | Leq | Geq), v, _ -> type_error (BaseType IntType) v

let eval_un_op op v =
  match (op, v) with
  | Neg, RTVal.Int i -> RSR.ok (RTVal.Int (Int64.neg i))
  | Neg, RTVal.Double d -> RSR.ok (RTVal.Double (-.d))
  | Not, RTVal.Bool b -> RSR.ok (RTVal.Bool (not b))
  | Neg, v -> type_error (BaseType IntType) v
  | Not, v -> type_error (BaseType BoolType) v

let eval_base_val Loc.{ it = v } =
  match v with
  | IntVal i -> RTVal.Int i
  | BoolVal b -> RTVal.Bool b
  | DoubleVal d -> RTVal.Double d
  | CharVal c -> RTVal.Char c

let eval_decl id v { env; mem } =
  let mem, addr = Mem.alloc v mem in
  let env = Env.add id (Addr addr) env in
  RSR.ok { env; mem }

let rec eval_expr Loc.{ it = e; at = loc } s =
  match e with
  | CVal v -> RSR.ok (eval_base_val v, s.mem)
  | CVar id -> RSR.ok (lookup_var id s, s.mem)
  | CBinOp (e1, ((And | Or) as op), e2) -> (
      let* v1, s = eval_expr e1 s =>> set_mem_snd s in
      match (op, v1) with
      | And, RTVal.Bool false -> RSR.ok (RTVal.Bool false, s.mem)
      | Or, RTVal.Bool true -> RSR.ok (RTVal.Bool true, s.mem)
      | _ ->
          let* v2, s = eval_expr e2 s =>> set_mem_snd s in
          let* v =
            eval_bin_op
              (Loc.with_loc ~loc:e1.at v1)
              op
              (Loc.with_loc ~loc:e2.at v2)
          in
          RSR.ok (v, s.mem))
  | CBinOp (e1, op, e2) ->
      let* v1, s = eval_expr e1 s =>> set_mem_snd s in
      let* v2, s = eval_expr e2 s =>> set_mem_snd s in
      let* v =
        eval_bin_op (Loc.with_loc ~loc:e1.at v1) op (Loc.with_loc ~loc:e2.at v2)
      in
      RSR.ok (v, s.mem)
  | CUnOp (op, e) ->
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let* v = eval_un_op op v in
      RSR.ok (v, s.mem)
  | CArrayLoad (id, e) ->
      let arr_ptr = get_arr_ptr id s in
      let* idx, s = eval_expr e s =>> set_mem_snd s in
      let idx = as_int ~expected:(type_error (BaseType IntType)) idx in
      let arr_len = get_arr_len id arr_ptr s in
      if Int64.compare idx 0L < 0 || Int64.compare idx arr_len >= 0 then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let v = lookup_addr (arr_elem_addr arr_ptr idx) s.mem in
        RSR.ok (v, s.mem)
  | CNewArrayDefault (t, e) ->
      let* n, s = eval_expr e s =>> set_mem_snd s in
      let n = as_int ~expected:(type_error (BaseType IntType)) n in
      if Int64.compare n 0L <= 0 then
        error_with_loc ~loc:e.at "la lunghezza di un array deve essere positiva"
      else
        let* len =
          if Int64.compare n (Int64.of_int max_int) > 0 then
            error_with_loc ~loc:e.at
              "lunghezza dell'array troppo grande (%Ld > %d)" n max_int
          else RSR.ok (Int64.to_int n)
        in
        let data = List.init len (fun _ -> RTVal.default_from_type t) in
        let arr_ptr, s = alloc_array data s.mem |> set_mem_snd s in
        RSR.ok (arr_ptr, s.mem)
  | CNewArrayLiteral vs ->
      let vs = List.map eval_base_val vs in
      let arr_ptr, s = alloc_array vs s.mem |> set_mem_snd s in
      RSR.ok (arr_ptr, s.mem)
  | CArrayLength id ->
      let arr_ptr = get_arr_ptr id s in
      let arr_len = get_arr_len id arr_ptr s in
      RSR.ok (RTVal.Int arr_len, s.mem)
  | CCast (t, e) ->
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let* v =
        match (t, v) with
        | BaseType IntType, (RTVal.Double d as v) ->
            (* the int range is [-2^63, 2^63), and NaN fails both tests *)
            let d = Float.floor d and bound = -.Int64.to_float Int64.min_int in
            if d >= -.bound && d < bound then
              RSR.ok (RTVal.Int (Int64.of_float d))
            else cast_range_error ~loc v
        | BaseType DoubleType, RTVal.Int i ->
            RSR.ok (RTVal.Double (Int64.to_float i))
        | _ -> cast_error v t
      in
      RSR.ok (v, s.mem)
  | CExprBlock (c, e) -> eval_command c s >>= eval_expr e
  | CCall (id, args) ->
      let params, body = lookup_fun id s.env in
      let decls = bind_params ~loc params args in
      let e = Loc.with_loc ~loc (CExprBlock (decls, body)) in
      let* () = enter_call ~loc in
      let* call_id = RSR.incr_call_id () in
      let* () = rsr_log ~loc s (EBeginCall (id, call_id)) in
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let* () = RSR.exit_call () in
      let* () = rsr_log ~loc s (EEndCall (id, call_id, v)) in
      RSR.ok (v, s.mem)
  | CTernary (e1, e2, e3) ->
      let* v1, s = eval_expr e1 s =>> set_mem_snd s in
      let b = as_bool ~expected:(type_error (BaseType BoolType)) v1 in
      if b then eval_expr e2 s else eval_expr e3 s

and eval_command (Loc.{ it = c; at = loc } as c_with_loc) s =
  match c with
  | CSkip -> RSR.ok s
  | CDeclare [ ((t, id), e) ] ->
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let* s = eval_decl id v s in
      let* () = rsr_log ~loc s (EDeclare (t, id, v)) in
      RSR.ok s
  | CDeclare (((t, id), e) :: ds) ->
      let ds = Loc.with_loc ~loc (CDeclare ds) in
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let* s = eval_command ds s >>= eval_decl id v in
      let* () = rsr_log ~loc s (EDeclare (t, id, v)) in
      RSR.ok s
  | CDeclare [] -> unreachable "dichiarazione senza variabili"
  | CAssign [ (id, e) ] ->
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let s = store_var id v s |> set_mem s in
      let* () = rsr_log ~loc s (EAssign (id, v)) in
      RSR.ok s
  | CAssign ((id, e) :: xs) ->
      let xs = Loc.with_loc ~loc (CAssign xs) in
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let* s = eval_command xs s =>> store_var id v =>> set_mem s in
      let* () = rsr_log ~loc s (EAssign (id, v)) in
      RSR.ok s
  | CAssign [] -> unreachable "assegnamento senza variabili"
  | CArrayStore (id, e1, e2) ->
      let arr_ptr = get_arr_ptr id s in
      let* idx, s = eval_expr e1 s =>> set_mem_snd s in
      let idx = as_int ~expected:(type_error (BaseType IntType)) idx in
      let* v, s = eval_expr e2 s =>> set_mem_snd s in
      let arr_len = get_arr_len id arr_ptr s in
      if Int64.compare idx 0L < 0 || Int64.compare idx arr_len >= 0 then
        index_out_of_bounds_error ~loc idx arr_len
      else
        let addr = arr_elem_addr arr_ptr idx in
        let s = Mem.set addr v s.mem |> set_mem s in
        let* () = rsr_log ~loc s (EArrayStore (id, idx, v, addr)) in
        RSR.ok s
  | CSeq (c1, c2) -> eval_command c1 s >>= eval_command c2
  | CIfThenElse (e, c1, c2) ->
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let b = as_bool ~expected:(type_error (BaseType BoolType)) v in
      let* () = rsr_log ~loc:e.at s (EIfThenElse b) in
      let* s =
        (if b then eval_command c1 s else eval_command c2 s) =>> replace_mem s
      in
      RSR.ok s
  | CWhile (e, c) ->
      let* v, s = eval_expr e s =>> set_mem_snd s in
      let b = as_bool ~expected:(type_error (BaseType BoolType)) v in
      let* () = rsr_log ~loc:e.at s (EWhile b) in
      if b then eval_command c s =>> replace_mem s >>= eval_command c_with_loc
      else RSR.ok s
  | CBlock c -> eval_command c s =>> replace_mem s
  | CProc (id, ds, c) ->
      let s = set_env s (Env.add id (LambdaProc (ds, c)) s.env) in
      let* () = rsr_log ~loc s (EProc id) in
      RSR.ok s
  | CInvoke (id, args) ->
      let params, body = lookup_proc id s.env in
      let decls = bind_params ~loc params args in
      let body = Loc.with_loc ~loc (CSeq (decls, body)) in
      let body = Loc.with_loc ~loc (CBlock body) in
      let* () = enter_call ~loc in
      let* call_id = RSR.incr_call_id () in
      let* () = rsr_log ~loc s (EBeginInvoke (id, call_id)) in
      let* s = eval_command body s =>> replace_mem s in
      let* () = RSR.exit_call () in
      let* () = rsr_log ~loc s (EEndInvoke (id, call_id)) in
      RSR.ok s
  | CFunc (id, ds, e) ->
      let s = Env.add id (LambdaFun (ds, e)) s.env |> set_env s in
      let* () = rsr_log ~loc s (EFunc id) in
      RSR.ok s

let run_program c = eval_command c { env = Env.empty; mem = Mem.empty }
