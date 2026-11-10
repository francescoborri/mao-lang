open MaoAst
open Utils

let pp_block pp fmt x = Fmt.pf fmt "{@;<0 4>@[<v>%a@]@;<0 0>}" pp x
let pp_wrapper pp = Fmt.vbox pp

let pp_base_type fmt = function
  | IntType -> Fmt.string fmt "int"
  | BoolType -> Fmt.string fmt "bool"
  | CharType -> Fmt.string fmt "char"
  | DoubleType -> Fmt.string fmt "double"

let pp_composite_type fmt = function
  | Array t -> Fmt.pf fmt "%a[]" pp_base_type t

let rec pp_func_type fmt (ts, t) =
  let pp_res fmt = function
    | None -> Fmt.string fmt "void"
    | Some t -> pp_mao_type fmt t
  in
  Fmt.pf fmt "(%a) -> %a" (Fmt.list ~sep:(Fmt.any ", ") pp_mao_type) ts pp_res t

and pp_mao_type fmt = function
  | BaseType t -> pp_base_type fmt t
  | CompositeType t -> pp_composite_type fmt t
  | FuncType (ts, t) -> pp_func_type fmt (ts, t)

let pp_typed_var fmt (t, id) = Fmt.pf fmt "%a %s" pp_mao_type t id

let pp_bin_op fmt = function
  | Add -> Fmt.string fmt "+"
  | Sub -> Fmt.string fmt "-"
  | Mul -> Fmt.string fmt "*"
  | Div -> Fmt.string fmt "/"
  | Mod -> Fmt.string fmt "%"
  | And -> Fmt.string fmt "&&"
  | Or -> Fmt.string fmt "||"
  | Xor -> Fmt.string fmt "xor"
  | Eq -> Fmt.string fmt "=="
  | Neq -> Fmt.string fmt "!="
  | Lt -> Fmt.string fmt "<"
  | Gt -> Fmt.string fmt ">"
  | Leq -> Fmt.string fmt "<="
  | Geq -> Fmt.string fmt ">="

let pp_base_val fmt Loc.{ it = v } =
  match v with
  | IntVal i -> Fmt.pf fmt "%d" i
  | BoolVal b -> Fmt.pf fmt "%b" b
  | CharVal c -> Fmt.pf fmt "'%c'" c
  | DoubleVal d -> Fmt.string fmt (shortest_float_string d)

let pp_un_op fmt = function
  | Neg -> Fmt.string fmt "-"
  | Not -> Fmt.string fmt "!"

let pp_decl_lhs fmt ds =
  match ds with
  | [] -> unreachable "Declaration must have at least one variable"
  | x :: [] -> pp_typed_var fmt x
  | (t, x) :: xs when List.for_all (fun (t', _) -> t = t') xs ->
      Fmt.pf fmt "%a %a" pp_mao_type t
        (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
        (x :: List.map snd xs)
  | _ -> Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any ", ") pp_typed_var) ds

let rec pp_expr fmt Loc.{ it = e } =
  let is_atomic = function
    | Var _ | Val _ | ArrayLoad _ | NewArrayDefault _ | NewArrayLiteral _
    | ArrayLength _ | Call _ | ExprBlock _ ->
        true
    | BinOp _ | UnOp _ | Cast _ | Ternary _ -> false
  in
  let pp_inner fmt e =
    if is_atomic (Loc.get_item e) then pp_expr fmt e
    else Fmt.pf fmt "(%a)" pp_expr e
  in
  match e with
  | Var id -> Fmt.string fmt id
  | Val v -> pp_base_val fmt v
  | BinOp (e1, op, e2) ->
      Fmt.pf fmt "%a %a %a" pp_inner e1 pp_bin_op op pp_inner e2
  | UnOp (op, e) -> Fmt.pf fmt "%a%a" pp_un_op op pp_inner e
  | ArrayLoad (id, e) -> Fmt.pf fmt "%s[%a]" id pp_expr e
  | NewArrayDefault (t, e) -> Fmt.pf fmt "new %a[%a]" pp_base_type t pp_expr e
  | NewArrayLiteral ({ it = CharVal c } :: _ as cs) ->
      let pp_base_val fmt Loc.{ it = c } =
        match c with
        | CharVal c -> Fmt.pf fmt "%c" c
        | _ -> unreachable "expected a char"
      in
      Fmt.pf fmt "\"%a\"" Fmt.(list ~sep:nop pp_base_val) cs
  | NewArrayLiteral vs ->
      Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") pp_base_val) vs
  | ArrayLength id -> Fmt.pf fmt "%s.length" id
  | Cast (t, e) -> Fmt.pf fmt "%a(%a)" pp_mao_type t pp_expr e
  | Call (id, es) ->
      Fmt.pf fmt "%s(%a)" id (Fmt.list ~sep:(Fmt.any ", ") pp_expr) es
  | ExprBlock (c, e) ->
      let pp_expr_block fmt (c, e) =
        Fmt.pf fmt "%a@,return %a;" pp_command c pp_inner e
      in
      (pp_block pp_expr_block) fmt (c, e)
  | Ternary (e1, e2, e3) ->
      Fmt.pf fmt "%a ? %a : %a" pp_inner e1 pp_inner e2 pp_inner e3

and pp_command fmt { it = c } =
  let pp_aux fmt c =
    match c with
    | Skip -> Fmt.string fmt "skip;"
    | Declare (ds, None) -> Fmt.pf fmt "%a;" pp_decl_lhs ds
    | Declare (ds, Some es) ->
        Fmt.pf fmt "%a = %a;" pp_decl_lhs ds
          (Fmt.list ~sep:(Fmt.any ", ") pp_expr)
          es
    | Assign (ids, es) ->
        Fmt.pf fmt "%a := %a;"
          (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
          ids
          (Fmt.list ~sep:(Fmt.any ", ") pp_expr)
          es
    | ArrayStore (id, e1, e2) ->
        Fmt.pf fmt "%s[%a] := %a;" id pp_expr e1 pp_expr e2
    | Seq (c1, c2) -> Fmt.pf fmt "%a@,%a" pp_command c1 pp_command c2
    | IfThenElse (e, c1, c2) ->
        Fmt.pf fmt "if (%a) %a else %a" pp_expr e (pp_block pp_command) c1
          (pp_block pp_command) c2
    | While (e, c) ->
        Fmt.pf fmt "while (%a) %a" pp_expr e (pp_block pp_command) c
    | Block c -> Fmt.pf fmt "%a" (pp_block pp_command) c
    | Proc (id, ds, c) ->
        Fmt.pf fmt "%s(%a) %a" id
          (Fmt.list ~sep:(Fmt.any ", ") pp_typed_var)
          ds (pp_block pp_command) c
    | Invoke (id, es) ->
        Fmt.pf fmt "%s(%a);" id (Fmt.list ~sep:(Fmt.any ", ") pp_expr) es
    | Func (id, ds, t, e) ->
        Fmt.pf fmt "%a %s(%a) %a" pp_mao_type t id
          (Fmt.list ~sep:(Fmt.any ", ") pp_typed_var)
          ds pp_expr e
  in
  pp_wrapper pp_aux fmt c
