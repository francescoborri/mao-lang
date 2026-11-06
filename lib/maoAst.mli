type base_type = IntType | BoolType | DoubleType | CharType
type composite_type = Array of base_type

type mao_type =
  | BaseType of base_type
  | CompositeType of composite_type
  | FuncType of mao_type list * mao_type option

type typed_var = mao_type * string

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq

type un_op = Neg | Not

type base_val_node =
  | IntVal of int
  | BoolVal of bool
  | DoubleVal of float
  | CharVal of char

type base_val = base_val_node Loc.t

type expr = expr_node Loc.t
and command = command_node Loc.t

and expr_node =
  | Val of base_val
  | Var of string
  | BinOp of expr * bin_op * expr
  | UnOp of un_op * expr
  | ArrayLoad of string * expr
  | NewArrayDefault of base_type * expr
  | NewArrayLiteral of base_val list
  | ArrayLength of string
  | Cast of mao_type * expr
  | Call of string * expr list
  | ExprBlock of command * expr
  | Ternary of expr * expr * expr

and command_node =
  | Skip
  | Declare of typed_var list * expr list
  | Assign of string list * expr list
  | ArrayStore of string * expr * expr
  | Seq of command * command
  | IfThenElse of expr * command * command
  | While of expr * command
  | Block of command
  | Proc of string * typed_var list * command
  | Invoke of string * expr list
  | Func of string * typed_var list * mao_type * expr
