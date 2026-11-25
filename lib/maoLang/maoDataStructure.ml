open MaoAst
open MaoAst_pp
open Utils

module type Addr = sig
  type t

  val zero : t
  val next : t -> t
  val offset : t -> Int64.t -> t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Int64Addr = struct
  type t = Int64.t

  let zero = 0L
  let next x = Int64.succ x
  let offset x n = Int64.add x n
  let compare = Int64.compare
  let pp fmt = Fmt.pf fmt "0x%Lx"
end

module MemMap = struct
  module type S = sig
    type addr
    type 'a t

    val empty : 'a t
    val alloc : 'a -> 'a t -> 'a t * addr
    val get : addr -> 'a t -> 'a option
    val set : addr -> 'a -> 'a t -> 'a t
    val next_addr : 'a t -> addr
    val to_seq_from : addr -> 'a t -> (addr * 'a) Seq.t
    val pp : 'a Fmt.t -> 'a t Fmt.t
  end

  module Make (Addr : Addr) : S with type addr = Addr.t = struct
    module M = Map.Make (Addr)

    type addr = Addr.t
    type 'a t = { map : 'a M.t; next_addr : Addr.t }

    let empty = { map = M.empty; next_addr = Addr.zero }

    let alloc v m =
      ( { map = M.add m.next_addr v m.map; next_addr = Addr.next m.next_addr },
        m.next_addr )

    let get addr m = M.find_opt addr m.map

    let set addr v m =
      let max x y = if Addr.compare x y > 0 then x else y in
      { map = M.add addr v m.map; next_addr = max m.next_addr (Addr.next addr) }

    let next_addr m = m.next_addr
    let to_seq_from addr m = M.to_seq_from addr m.map

    let pp pp_v fmt m =
      let pp_binding fmt (addr, v) =
        Fmt.pf fmt "%a -> %a" Addr.pp addr pp_v v
      in
      Fmt.pf fmt "{@[<v>%a@]}"
        Fmt.(list ~sep:(any ";@ ") pp_binding)
        (M.bindings m.map)
  end
end

module RTVal = struct
  type t =
    | Int of Int64.t
    | Bool of bool
    | Double of float
    | Char of char
    | Addr of Int64Addr.t

  let default_from_type = function
    | IntType -> Int 0L
    | BoolType -> Bool false
    | DoubleType -> Double 0.0
    | CharType -> Char '\000'

  let pp fmt = function
    | Int i -> Fmt.string fmt (Int64.to_string i)
    | Bool b -> Fmt.pf fmt "%b" b
    | Double d -> Fmt.string fmt (shortest_float_string d)
    | Char c -> Fmt.pf fmt "'%s'" (escape_char c)
    | Addr addr -> Int64Addr.pp fmt addr

  let pp_type fmt = function
    | Int _ -> Fmt.pf fmt "%a" pp_base_type IntType
    | Bool _ -> Fmt.pf fmt "%a" pp_base_type BoolType
    | Double _ -> Fmt.pf fmt "%a" pp_base_type DoubleType
    | Char _ -> Fmt.pf fmt "%a" pp_base_type CharType
    | Addr _ -> Fmt.string fmt "array"
end

module StringSet = Set.Make (String)
module Env = Map.Make (String)
module Mem = MemMap.Make (Int64Addr)

type binding =
  | Addr of Int64Addr.t
  | LambdaFun of typed_var list * checked_expr * env
  | LambdaProc of typed_var list * checked_command * env

and env = binding Env.t

type mem = RTVal.t Mem.t
type state = { env : env; mem : mem }

type event =
  | EDeclare of mao_type * string * RTVal.t
  | EAssign of string * RTVal.t
  | EArrayStore of string * Int64.t * RTVal.t * Int64Addr.t
  | EIfThenElse of bool
  | EWhile of bool
  | EFunc of string
  | EProc of string
  | EBeginCall of string * int
  | EEndCall of string * int * RTVal.t
  | EBeginInvoke of string * int
  | EEndInvoke of string * int

type log = { state : state; event : event }
