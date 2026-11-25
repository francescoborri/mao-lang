open MaoAst_pp
open MaoDataStructure
open MaoDataStructure_pp
open Utils
module Interpreter = MaoInterpreter

module Kind = struct
  type t = Decl | Assign | Store | Branch | Def | Call | Invoke

  let all = [ Decl; Assign; Store; Branch; Def; Call; Invoke ]

  let of_event = function
    | EDeclare _ -> Decl
    | EAssign _ -> Assign
    | EArrayStore _ -> Store
    | EIfThenElse _ | EWhile _ -> Branch
    | EFunc _ | EProc _ -> Def
    | EBeginCall _ | EEndCall _ -> Call
    | EBeginInvoke _ | EEndInvoke _ -> Invoke

  let of_log Loc.{ it = { event } } = of_event event

  let to_string = function
    | Decl -> "decl"
    | Assign -> "assign"
    | Store -> "store"
    | Branch -> "branch"
    | Def -> "def"
    | Call -> "call"
    | Invoke -> "invoke"

  let of_string s = List.find_opt (fun kind -> to_string kind = s) all
end

module Entry = struct
  type t = {
    index : int;
    kind : Kind.t;
    name : string;
    descr : string;
    depth : int;
    loc : Loc.loc;
  }
end

module Var = struct
  type t = {
    name : string;
    ty : string;
    value : string;
    array : (Int64Addr.t * string) option;
    addr : Int64Addr.t option;
    changed : bool;
  }
end

module Cell = struct
  type t = {
    addr : Int64Addr.t;
    value : string;
    ty : string;
    owner : string;
    changed : bool;
  }
end

module Page = struct
  type 'a t = { items : 'a list; offset : int; total : int }

  (* an [offset] past the end selects the last page *)
  let window ~offset ~limit ~total =
    let limit = max 1 limit in
    let offset =
      if offset < total then max 0 offset else max 0 (total - 1) / limit * limit
    in
    (offset, min limit (total - offset))

  let init ~offset ~limit ~total nth =
    let offset, n = window ~offset ~limit ~total in
    { items = List.init n (fun k -> nth (offset + k)); offset; total }

  let of_list ~offset ~limit xs =
    let total = List.length xs in
    let offset, n = window ~offset ~limit ~total in
    let items =
      List.to_seq xs |> Seq.drop offset |> Seq.take n |> List.of_seq
    in
    { items; offset; total }
end

module Region = struct
  type t =
    | Variable of string * Int64Addr.t
    | Length of string * Int64Addr.t
    | Elements of string * Int64Addr.t * Int64.t

  let of_binding ({ mem } as s) = function
    | id, Addr addr -> (
        match Interpreter.lookup_addr addr mem with
        | RTVal.Addr arr_ptr ->
            let arr_len = Interpreter.get_arr_len id arr_ptr s in
            [
              Variable (id, addr);
              Length (id, arr_ptr);
              Elements (id, arr_ptr, arr_len);
            ]
        | _ -> [ Variable (id, addr) ])
    | _, (LambdaFun _ | LambdaProc _) -> []

  let owner addr = function
    | Variable (id, addr') when Int64Addr.compare addr addr' = 0 -> Some id
    | Length (id, arr_ptr)
      when Int64Addr.compare addr (Interpreter.arr_len_addr arr_ptr) = 0 ->
        Some (id ^ ".length")
    | Elements (id, arr_ptr, arr_len) ->
        let idx = Int64.sub addr (Interpreter.arr_elem_addr arr_ptr 0L) in
        if Int64.compare idx 0L >= 0 && Int64.compare idx arr_len < 0 then
          Some (Fmt.str "%s[%Ld]" id idx)
        else None
    | Variable _ | Length _ -> None
end

let contains ~sub s =
  let n, m = (String.length s, String.length sub) in
  let occurs_at i = String.sub s i m = sub in
  m <= n && Seq.exists occurs_at (Seq.init (n - m + 1) Fun.id)

let normalize_query = String.trim >> String.lowercase_ascii

let matches ~query fields =
  String.concat " " fields |> String.lowercase_ascii |> contains ~sub:query

let preview_length = 8

let pp_array_preview s fmt (id, arr_ptr) =
  let arr_len = Interpreter.get_arr_len id arr_ptr s |> Int64.to_int in
  let elems =
    Mem.to_seq_from (Interpreter.arr_elem_addr arr_ptr 0L) s.mem
    |> Seq.take (min arr_len preview_length)
    |> Seq.map snd |> List.of_seq
  in
  Fmt.pf fmt "lunghezza %d: [%a%s]" arr_len
    (Fmt.list ~sep:(Fmt.any ", ") RTVal.pp)
    elems
    (if arr_len > preview_length then ", ..." else "")

let pp_signature fmt (id, params) =
  Fmt.pf fmt "%s(%a)" id (Fmt.list ~sep:(Fmt.any ", ") pp_typed_var) params

(* arrays are never empty, so the first element gives the element type *)
let pp_value_type mem fmt = function
  | RTVal.Addr arr_ptr ->
      Fmt.pf fmt "%a[]" RTVal.pp_type
        (Interpreter.lookup_addr (Interpreter.arr_elem_addr arr_ptr 0L) mem)
  | v -> RTVal.pp_type fmt v

let var_of_binding ({ mem } as s) (name, binding) =
  let var ?array ?addr ty value =
    Var.{ name; ty; value; array; addr; changed = false }
  in
  match binding with
  | Addr addr -> (
      let v = Interpreter.lookup_addr addr mem in
      let ty = Fmt.str "%a" (pp_value_type mem) v in
      match v with
      | RTVal.Addr arr_ptr ->
          let preview = Fmt.str "%a" (pp_array_preview s) (name, arr_ptr) in
          var ~addr ~array:(arr_ptr, preview) ty (Fmt.str "%a" RTVal.pp v)
      | v -> var ~addr ty (Fmt.str "%a" RTVal.pp v))
  | LambdaFun (params, _, _) ->
      var "funzione" (Fmt.str "%a" pp_signature (name, params))
  | LambdaProc (params, _, _) ->
      var "procedura" (Fmt.str "%a" pp_signature (name, params))

let var_of ~prev s (name, binding) =
  let var = var_of_binding s (name, binding) in
  let prev_var =
    Env.find_opt name prev.env
    |> Option.map (fun binding -> var_of_binding prev (name, binding))
  in
  { var with Var.changed = prev_var <> Some var }

let cell_of ~regions ~prev { mem } (addr, v) =
  Cell.
    {
      addr;
      value = Fmt.str "%a" RTVal.pp v;
      ty = Fmt.str "%a" (pp_value_type mem) v;
      owner =
        regions |> List.find_map (Region.owner addr) |> Option.value ~default:"";
      changed = Mem.get addr prev.mem <> Some v;
    }

(* [texts] caches the formatted event of each log, since filtering by text
   formats all of them *)
type t = {
  logs : log Loc.t array;
  depths : int array;
  texts : (string * string) Lazy.t array;
  view : int array;
}

let call_depths logs =
  let depth_of depth Loc.{ it = { event } } =
    match event with
    | EBeginCall _ | EBeginInvoke _ -> (depth + 1, depth)
    | EEndCall _ | EEndInvoke _ -> (depth - 1, depth - 1)
    | _ -> (depth, depth)
  in
  Array.fold_left_map depth_of 0 logs |> snd

let texts_of Loc.{ it = { event } } =
  (Fmt.str "%a" pp_event_name event, Fmt.str "%a" pp_event_descr event)

let of_logs logs =
  let logs = Array.of_list logs in
  {
    logs;
    depths = call_depths logs;
    texts = Array.map (fun log -> lazy (texts_of log)) logs;
    view = Array.init (Array.length logs) Fun.id;
  }

let length { logs } = Array.length logs
let view_length { view } = Array.length view
let max_depth { depths } = Array.fold_left max 0 depths

let kind_counts { logs } =
  let count kind =
    Array.to_seq logs |> Seq.filter (Kind.of_log >> ( = ) kind) |> Seq.length
  in
  List.map (fun kind -> (kind, count kind)) Kind.all

let entry { logs; depths; texts } index =
  let (Loc.{ at = loc } as log) = logs.(index) in
  let name, descr = Lazy.force texts.(index) in
  Entry.
    { index; kind = Kind.of_log log; name; descr; depth = depths.(index); loc }

let entries ({ view } as t) ~offset ~limit =
  Page.init ~offset ~limit ~total:(Array.length view) (Array.get view >> entry t)

let filter ({ logs; texts } as t) ~kinds ~query =
  let query = normalize_query query in
  let text_matches (name, descr) = matches ~query [ name; descr ] in
  let keep index =
    List.mem (Kind.of_log logs.(index)) kinds
    && (query = "" || text_matches (Lazy.force texts.(index)))
  in
  {
    t with
    view = Seq.init (length t) Fun.id |> Seq.filter keep |> Array.of_seq;
  }

(* position in the view of the last log not after [index] *)
let position { view } index =
  let rec upper_bound lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if view.(mid) <= index then upper_bound (mid + 1) hi
      else upper_bound lo mid
  in
  max 0 (upper_bound 0 (Array.length view) - 1)

let states_at { logs } index =
  let state_at index = (Loc.get_item logs.(index)).state in
  let prev =
    if index > 0 then state_at (index - 1)
    else { env = Env.empty; mem = Mem.empty }
  in
  (state_at index, prev)

let vars t index ~query ~offset ~limit =
  let s, prev = states_at t index in
  let query = normalize_query query in
  let var_matches Var.{ name; ty; value; array } =
    matches ~query [ name; ty; value; Option.fold ~none:"" ~some:snd array ]
  in
  Env.bindings s.env
  |> List.map (var_of ~prev s)
  |> List.filter var_matches
  |> Page.of_list ~offset ~limit

let memory t index ~query ~offset ~limit =
  let s, prev = states_at t index in
  let regions = Env.bindings s.env |> List.concat_map (Region.of_binding s) in
  let cell = cell_of ~regions ~prev s in
  let cell_at addr = cell (addr, Interpreter.lookup_addr addr s.mem) in
  match normalize_query query with
  | "" ->
      let total = Int64.to_int (Mem.next_addr s.mem) in
      Page.init ~offset ~limit ~total (Int64.of_int >> cell_at)
  | query ->
      Mem.to_seq_from Int64Addr.zero s.mem
      |> Seq.map cell
      |> Seq.filter (fun Cell.{ value; owner } ->
          matches ~query [ value; owner ])
      |> List.of_seq
      |> Page.of_list ~offset ~limit
