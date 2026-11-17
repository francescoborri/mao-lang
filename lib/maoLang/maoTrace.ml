(* Query-able view over the logs of a run, used by the web playground.

   Logs hold persistent states, so consecutive logs share most of their memory:
   every query returns a single page of entries, variables or memory cells, and
   never materialises a whole state. *)

open MaoAst_pp
open MaoDataStructure
open MaoDataStructure_pp
open Utils

module Kind = struct
  type t = Decl | Assign | Store | Branch | Def | Call | Invoke

  let all = [ Decl; Assign; Store; Branch; Def; Call; Invoke ]

  let of_event = function
    | EDeclare _ | EDeclares _ -> Decl
    | EAssign _ | EAssigns _ -> Assign
    | EArrayStore _ -> Store
    | EIfThenElse _ | EWhile _ -> Branch
    | EFunc _ | EProc _ -> Def
    | EBeginCall _ | EEndCall _ -> Call
    | EBeginInvoke _ | EEndInvoke _ -> Invoke

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
    array : (IntAddr.t * string) option;
        (** address and preview of the array the variable points to *)
    addr : IntAddr.t option;  (** [None] for functions and procedures *)
    changed : bool;
  }
end

module Cell = struct
  type t = {
    addr : IntAddr.t;
    value : string;
    ty : string;
    owner : string;  (** variable (or array element) in scope stored here *)
    changed : bool;
  }
end

type 'a page = { items : 'a list; offset : int; total : int }

type t = {
  logs : log Loc.t array;
  depths : int array;
  texts : (string * string) Lazy.t array;  (** event name and description *)
  view : int array;  (** indices of the logs passing the current filter *)
}

(* ------------------------------------------------------------- helpers *)

(* The page of at most [limit] items starting at [offset], or the last page
   when [offset] is past the end. *)
let page ~offset ~limit ~total nth =
  let limit = max 1 limit in
  let offset =
    if offset < total then max 0 offset else max 0 (total - 1) / limit * limit
  in
  let items =
    List.init (min limit (total - offset)) (fun k -> nth (offset + k))
  in
  { items; offset; total }

let page_of_list ~offset ~limit xs =
  let xs = Array.of_list xs in
  page ~offset ~limit ~total:(Array.length xs) (Array.get xs)

let contains ~sub s =
  let n, m = (String.length s, String.length sub) in
  let rec occurs_at i j =
    j = m || (s.[i + j] = sub.[j] && occurs_at i (j + 1))
  in
  let rec search i = i + m <= n && (occurs_at i 0 || search (i + 1)) in
  search 0

let normalize_query = String.trim >> String.lowercase_ascii

let matches ~query fields =
  contains ~sub:query (String.lowercase_ascii (String.concat " " fields))

let initial_state = { env = Env.empty; mem = Mem.empty }
let state_at { logs } i = logs.(i).Loc.it.state
let prev_state_at t i = if i > 0 then state_at t (i - 1) else initial_state

(* --------------------------------------------------------------- trace *)

let pp_event_label fmt = function
  | EDeclare _ | EDeclares _ -> Fmt.string fmt "dichiarazione"
  | EAssign _ | EAssigns _ -> Fmt.string fmt "assegnamento"
  | EArrayStore (_, _, _, addr) -> Fmt.pf fmt "scrittura su %a" IntAddr.pp addr
  | EIfThenElse _ -> Fmt.string fmt "if"
  | EWhile _ -> Fmt.string fmt "while"
  | EFunc _ -> Fmt.string fmt "dichiara funzione"
  | EProc _ -> Fmt.string fmt "dichiara procedura"
  | EBeginCall (_, call_id) -> Fmt.pf fmt "inizio chiamata #%d" call_id
  | EEndCall (_, call_id, _) -> Fmt.pf fmt "fine chiamata #%d" call_id
  | EBeginInvoke (_, call_id) -> Fmt.pf fmt "inizio invocazione #%d" call_id
  | EEndInvoke (_, call_id) -> Fmt.pf fmt "fine invocazione #%d" call_id

(* Call depth of each log: the begin and end entries of a call are at the
   depth of the caller, the entries in between one level deeper. *)
let depths logs =
  let depth_of depth Loc.{ it = { event } } =
    match event with
    | EBeginCall _ | EBeginInvoke _ -> (depth + 1, depth)
    | EEndCall _ | EEndInvoke _ -> (depth - 1, depth - 1)
    | _ -> (depth, depth)
  in
  Array.fold_left_map depth_of 0 logs |> snd

let texts_of Loc.{ it = { event } } =
  (Fmt.str "%a" pp_event_label event, Fmt.str "%a" pp_event_descr event)

let of_logs logs =
  let logs = Array.of_list logs in
  {
    logs;
    depths = depths logs;
    texts = Array.map (fun log -> lazy (texts_of log)) logs;
    view = Array.init (Array.length logs) Fun.id;
  }

let length { logs } = Array.length logs
let max_depth { depths } = Array.fold_left max 0 depths
let kind_at { logs } i = Kind.of_event logs.(i).Loc.it.event

let kind_counts t =
  let count kind =
    Seq.init (length t) (kind_at t) |> Seq.filter (( = ) kind) |> Seq.length
  in
  List.map (fun kind -> (kind, count kind)) Kind.all

let entry ({ logs; depths; texts } as t) index =
  let name, descr = Lazy.force texts.(index) in
  Entry.
    {
      index;
      kind = kind_at t index;
      name;
      descr;
      depth = depths.(index);
      loc = logs.(index).Loc.at;
    }

let filter t ~kinds ~query =
  let query = normalize_query query in
  let keep i =
    List.mem (kind_at t i) kinds
    && (query = ""
       ||
       let name, descr = Lazy.force t.texts.(i) in
       matches ~query [ name; descr ])
  in
  {
    t with
    view = Seq.init (length t) Fun.id |> Seq.filter keep |> Array.of_seq;
  }

let view_length { view } = Array.length view

let entries ({ view } as t) ~offset ~limit =
  page ~offset ~limit ~total:(Array.length view) (fun pos -> entry t view.(pos))

(* Position in the view of the last entry up to log [index], or of the first
   entry if there is none: where the selection lands when the filter changes. *)
let position { view } index =
  let rec count_up_to lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if view.(mid) <= index then count_up_to (mid + 1) hi
      else count_up_to lo mid
  in
  max 0 (count_up_to 0 (Array.length view) - 1)

(* --------------------------------------------------------- environment *)

let preview_length = 8
let lookup_addr = MaoInterpreter.lookup_addr
let arr_len_addr = MaoInterpreter.arr_len_addr
let arr_elem_addr = MaoInterpreter.arr_elem_addr

(* [Some (arr_ptr, arr_len)] when the cell at [addr] points to an array *)
let array_at addr mem =
  match Mem.get addr mem with
  | Some (RTVal.Addr arr_ptr) -> (
      match Mem.get (arr_len_addr arr_ptr) mem with
      | Some (RTVal.Int arr_len) -> Some (arr_ptr, arr_len)
      | _ -> None)
  | _ -> None

let pp_array_preview mem fmt (arr_ptr, arr_len) =
  let elems =
    List.init (min arr_len preview_length) (fun idx ->
        lookup_addr (arr_elem_addr arr_ptr idx) mem)
  in
  Fmt.pf fmt "lunghezza %d: [%a%s]" arr_len
    (Fmt.list ~sep:(Fmt.any ", ") RTVal.pp)
    elems
    (if arr_len > preview_length then ", ..." else "")

let pp_signature fmt (id, params) =
  Fmt.pf fmt "%s(%a)" id (Fmt.list ~sep:(Fmt.any ", ") pp_typed_var) params

let var_of { mem } (name, binding) =
  let var ?array ?addr ty value =
    Var.{ name; ty; value; array; addr; changed = false }
  in
  match binding with
  | Addr addr -> (
      let v = lookup_addr addr mem in
      match array_at addr mem with
      | Some ((arr_ptr, _) as arr) ->
          let elem = lookup_addr (arr_elem_addr arr_ptr 0) mem in
          var ~addr
            ~array:(arr_ptr, Fmt.str "%a" (pp_array_preview mem) arr)
            (Fmt.str "%a[]" RTVal.pp_type elem)
            (Fmt.str "%a" RTVal.pp v)
      | None ->
          var ~addr (Fmt.str "%a" RTVal.pp_type v) (Fmt.str "%a" RTVal.pp v))
  | LambdaFun (params, _) ->
      var "funzione" (Fmt.str "%a" pp_signature (name, params))
  | LambdaProc (params, _) ->
      var "procedura" (Fmt.str "%a" pp_signature (name, params))

(* The variables in scope after log [i]; a variable is changed when it was
   not in scope, or was shown differently, after the previous log. *)
let vars t i ~query ~offset ~limit =
  let state, prev = (state_at t i, prev_state_at t i) in
  let query = normalize_query query in
  let with_changed (name, binding) =
    let var = var_of state (name, binding) in
    let prev_var =
      Env.find_opt name prev.env |> Option.map (fun b -> var_of prev (name, b))
    in
    { var with Var.changed = prev_var <> Some var }
  in
  let var_matches Var.{ name; ty; value; array } =
    matches ~query [ name; ty; value; Option.fold ~none:"" ~some:snd array ]
  in
  Env.bindings state.env |> List.map with_changed |> List.filter var_matches
  |> page_of_list ~offset ~limit

(* -------------------------------------------------------------- memory *)

(* The memory regions owned by the variables in scope, as (first, last, label)
   triples: the cell of each variable and, for arrays, their length and
   elements. *)
let regions { env; mem } =
  Env.bindings env
  |> List.concat_map (function
    | name, Addr addr -> (
        let var = (addr, addr, Fun.const name) in
        match array_at addr mem with
        | Some (arr_ptr, arr_len) ->
            let first, last =
              (arr_elem_addr arr_ptr 0, arr_elem_addr arr_ptr (arr_len - 1))
            in
            [
              var;
              (arr_ptr, arr_ptr, Fun.const (name ^ ".length"));
              (first, last, fun addr -> Fmt.str "%s[%d]" name (addr - first));
            ]
        | None -> [ var ])
    | _, (LambdaFun _ | LambdaProc _) -> [])

let owner regions addr =
  regions
  |> List.find_map (fun (first, last, label) ->
      if first <= addr && addr <= last then Some (label addr) else None)
  |> Option.value ~default:""

let cell_of ~regions ~prev (addr, v) =
  Cell.
    {
      addr;
      value = Fmt.str "%a" RTVal.pp v;
      ty = Fmt.str "%a" RTVal.pp_type v;
      owner = owner regions addr;
      changed = Mem.get addr prev.mem <> Some v;
    }

(* The memory after log [i]. Without a query, pages go by address and only
   the cells in the page are visited; with a query, the whole memory is. *)
let memory t i ~query ~offset ~limit =
  let state, prev = (state_at t i, prev_state_at t i) in
  let cell = cell_of ~regions:(regions state) ~prev in
  let cell_matches ~query Cell.{ value; owner } =
    matches ~query [ value; owner ]
  in
  match normalize_query query with
  | "" ->
      page ~offset ~limit ~total:(Mem.next_addr state.mem) (fun addr ->
          cell (addr, lookup_addr addr state.mem))
  | query ->
      Mem.to_seq_from IntAddr.zero state.mem
      |> Seq.map cell
      |> Seq.filter (cell_matches ~query)
      |> List.of_seq
      |> page_of_list ~offset ~limit
