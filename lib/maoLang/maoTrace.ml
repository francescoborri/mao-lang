(* Trace session: an indexed, query-able view over the logs of one run.

   Design notes
   - Logs hold *persistent* states (Map-based), so consecutive logs share almost
     all of their memory. Nothing here ever materialises a whole state: every
     query returns a bounded window (a page of logs, a page of memory cells, ...).
   - Per-log metadata (call depth, matching end of a call, ...) is computed once
     in O(#logs). Event texts are formatted lazily and cached, because
     formatting through Fmt is by far the most expensive operation.
   - This module is JS-agnostic so that it can be unit-tested natively. *)

open MaoDataStructure
open MaoDataStructure_pp

type text = { name : string; descr : string; key : string }

type t = {
  logs : log Loc.t array;
  depth : int array;
  call_end : int array;  (** begin-of-call index -> end-of-call index, or -1 *)
  parent : int array;  (** innermost enclosing begin-of-call index, or -1 *)
  texts : text option array;  (** lazily computed, cached *)
  mutable view : int array option;  (** current filter result; None = all *)
}

type row = {
  index : int;
  kind : string;
  name : string;
  descr : string;
  loc : Loc.loc;
  depth : int;
  call : int;  (** call id for begin/end-of-call entries, -1 otherwise *)
  call_end : int;  (** for a begin entry: index of its end entry, else -1 *)
  out_end : int;  (** end entry of the innermost enclosing call, or -1 *)
}

type env_row = {
  var : string;
  vkind : string;  (** "var" | "array" | "func" | "proc" *)
  addr : int;  (** -1 when not applicable *)
  value : string;
  ty : string;
  detail : string;
}

type cell = {
  caddr : int;
  cvalue : string;
  cty : string;
  changed : bool;  (** differs from the previous log's memory *)
  label : string;  (** variable / array element owning the cell, or "" *)
}

type mem_page = { bound : int; start : int; cells : cell list }

(* ---------------------------------------------------------------- events *)

let all_kinds = [ "decl"; "assign"; "store"; "branch"; "def"; "call"; "invoke" ]

let kind_of_event = function
  | EDeclare _ | EDeclares _ -> "decl"
  | EAssign _ | EAssigns _ -> "assign"
  | EArrayStore _ -> "store"
  | EIfThenElse _ | EWhile _ -> "branch"
  | EFunc _ | EProc _ -> "def"
  | EBeginCall _ | EEndCall _ -> "call"
  | EBeginInvoke _ | EEndInvoke _ -> "invoke"

let call_of_event = function
  | EBeginCall (_, c)
  | EEndCall (_, c, _)
  | EBeginInvoke (_, c)
  | EEndInvoke (_, c) ->
      c
  | _ -> -1

let is_begin = function EBeginCall _ | EBeginInvoke _ -> true | _ -> false
let is_end = function EEndCall _ | EEndInvoke _ -> true | _ -> false
let event_at t i = t.logs.(i).Loc.it.event
let state_at t i = t.logs.(i).Loc.it.state

(* ---------------------------------------------------------------- build *)

let of_logs (logs : log Loc.t list) =
  let logs = Array.of_list logs in
  let n = Array.length logs in
  let depth = Array.make n 0 in
  let call_end = Array.make n (-1) in
  let parent = Array.make n (-1) in
  let stack = ref [] and d = ref 0 in
  let top () = match !stack with p :: _ -> p | [] -> -1 in
  Array.iteri
    (fun i (l : log Loc.t) ->
      let ev = l.Loc.it.event in
      if is_begin ev then (
        parent.(i) <- top ();
        depth.(i) <- !d;
        incr d;
        stack := i :: !stack)
      else if is_end ev then (
        (match !stack with _ :: tl -> stack := tl | [] -> ());
        d := max 0 (!d - 1);
        let c = call_of_event ev in
        if c >= 0 && c < n then call_end.(c) <- i;
        parent.(i) <- top ();
        depth.(i) <- !d)
      else (
        parent.(i) <- top ();
        depth.(i) <- !d))
    logs;
  { logs; depth; call_end; parent; texts = Array.make n None; view = None }

let length t = Array.length t.logs

let kind_counts t =
  let tbl = Hashtbl.create 8 in
  Array.iter
    (fun (l : log Loc.t) ->
      let k = kind_of_event l.Loc.it.event in
      Hashtbl.replace tbl k
        (1 + Option.value ~default:0 (Hashtbl.find_opt tbl k)))
    t.logs;
  List.map
    (fun k -> (k, Option.value ~default:0 (Hashtbl.find_opt tbl k)))
    all_kinds

let max_depth (t : t) = Array.fold_left max 0 t.depth

(* ---------------------------------------------------------------- texts *)

let text t i =
  match t.texts.(i) with
  | Some x -> x
  | None ->
      let ev = event_at t i in
      let name = Fmt.to_to_string pp_event_name ev in
      let descr = Fmt.to_to_string pp_event_descr ev in
      let x =
        { name; descr; key = String.lowercase_ascii (name ^ " " ^ descr) }
      in
      t.texts.(i) <- Some x;
      x

let row (t : t) i =
  let ev = event_at t i in
  let ({ name; descr; _ } : text) = text t i in
  let p = t.parent.(i) in
  {
    index = i;
    kind = kind_of_event ev;
    name;
    descr;
    loc = t.logs.(i).Loc.at;
    depth = t.depth.(i);
    call = call_of_event ev;
    call_end = t.call_end.(i);
    out_end = (if p < 0 then -1 else t.call_end.(p));
  }

(* --------------------------------------------------------- view / filter *)

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec at i j = j = m || (s.[i + j] = sub.[j] && at i (j + 1)) in
  let rec go i = i + m <= n && (at i 0 || go (i + 1)) in
  m = 0 || go 0

(** Restrict the view to entries whose kind is in [kinds] and whose text
    contains [text] (case-insensitive). Returns the number of matches. *)
let set_filter t ~kinds ~text:q =
  let q = String.lowercase_ascii (String.trim q) in
  let n = Array.length t.logs in
  if q = "" && List.for_all (fun k -> List.mem k kinds) all_kinds then (
    t.view <- None;
    n)
  else
    let res = Array.make n 0 and k = ref 0 in
    for i = 0 to n - 1 do
      if
        List.mem (kind_of_event (event_at t i)) kinds
        && (q = "" || contains ~sub:q (text t i).key)
      then (
        res.(!k) <- i;
        incr k)
    done;
    t.view <- Some (Array.sub res 0 !k);
    !k

let view_length t =
  match t.view with None -> Array.length t.logs | Some v -> Array.length v

let index_at t pos = match t.view with None -> pos | Some v -> v.(pos)

(** Position of log [i] in the current view when present (>= 0). Otherwise
    [-p - 1], where [p] is the position at which it would be inserted (so the
    entries around a filtered-out log are at [p - 1] and [p]). *)
let locate t i =
  let n = Array.length t.logs in
  match t.view with
  | None -> if i < 0 then -1 else if i >= n then -n - 1 else i
  | Some v ->
      let rec bs lo hi =
        if lo > hi then -lo - 1
        else
          let mid = (lo + hi) / 2 in
          if v.(mid) = i then mid
          else if v.(mid) < i then bs (mid + 1) hi
          else bs lo (mid - 1)
      in
      bs 0 (Array.length v - 1)

let page t ~offset ~limit =
  let len = view_length t in
  let offset = max 0 (min offset len) in
  let stop = min len (offset + max 0 limit) in
  List.init (stop - offset) (fun k -> row t (index_at t (offset + k)))

(* ---------------------------------------------------------- environment *)

let show v = Fmt.to_to_string RTVal.pp v
let type_name v = Fmt.to_to_string RTVal.pp_type v

let sig_of name params =
  Fmt.str "%s(%a)" name
    (Fmt.list ~sep:(Fmt.any ", ") MaoAst_pp.pp_typed_var)
    params

let array_len mem p =
  match Mem.get p mem with
  | Some (RTVal.Int n) when n >= 0 -> Some n
  | _ -> None

let array_elem_type mem p =
  match Mem.get (p + 1) mem with Some v -> type_name v | None -> "?"

let array_preview mem p n =
  let k = min n 8 in
  let elems =
    List.init k (fun j ->
        match Mem.get (p + 1 + j) mem with Some v -> show v | None -> "?")
  in
  Printf.sprintf "length %d: [%s%s]" n (String.concat ", " elems)
    (if n > k then ", ..." else "")

let env_rows t i =
  let { env; mem } = state_at t i in
  List.map
    (fun (var, b) ->
      match b with
      | Addr a -> (
          match Mem.get a mem with
          | Some (RTVal.Addr p as v) -> (
              match array_len mem p with
              | Some n ->
                  {
                    var;
                    vkind = "array";
                    addr = a;
                    value = show v;
                    ty = array_elem_type mem p ^ "[]";
                    detail = array_preview mem p n;
                  }
              | None ->
                  {
                    var;
                    vkind = "var";
                    addr = a;
                    value = show v;
                    ty = "array";
                    detail = "";
                  })
          | Some v ->
              {
                var;
                vkind = "var";
                addr = a;
                value = show v;
                ty = type_name v;
                detail = "";
              }
          | None ->
              {
                var;
                vkind = "var";
                addr = a;
                value = "<undefined>";
                ty = "?";
                detail = "";
              })
      | LambdaFun (params, _) ->
          {
            var;
            vkind = "func";
            addr = -1;
            value = sig_of var params;
            ty = "function";
            detail = "";
          }
      | LambdaProc (params, _) ->
          {
            var;
            vkind = "proc";
            addr = -1;
            value = sig_of var params;
            ty = "procedure";
            detail = "";
          })
    (Env.bindings env)

(* --------------------------------------------------------------- memory *)

(* Address ranges owned by variables of the current environment. The env is
   small, so labelling a page costs O(page * #vars) regardless of memory size. *)
let label_ranges { env; mem } =
  Env.bindings env
  |> List.concat_map (fun (name, b) ->
      match b with
      | Addr a -> (
          let own = (a, a, fun _ -> name) in
          match Mem.get a mem with
          | Some (RTVal.Addr p) -> (
              match array_len mem p with
              | Some n ->
                  [
                    own;
                    (p, p, fun _ -> name ^ ".length");
                    ( p + 1,
                      p + n,
                      fun x -> Printf.sprintf "%s[%d]" name (x - p - 1) );
                  ]
              | None -> [ own ])
          | _ -> [ own ])
      | LambdaFun _ | LambdaProc _ -> [])

let label_of ranges a =
  match List.find_opt (fun (lo, hi, _) -> a >= lo && a <= hi) ranges with
  | Some (_, _, f) -> f a
  | None -> ""

let make_cell ~ranges ~prev a v =
  let changed =
    match prev with None -> true | Some pm -> Mem.get a pm <> Some v
  in
  {
    caddr = a;
    cvalue = show v;
    cty = type_name v;
    changed;
    label = label_of ranges a;
  }

let prev_mem t i = if i > 0 then Some (state_at t (i - 1)).mem else None

let mem_page t ~log ~start ~count =
  let st = state_at t log in
  let start = max 0 start in
  let ranges = label_ranges st and prev = prev_mem t log in
  let cells =
    Mem.to_seq_from start st.mem
    |> Seq.take (max 0 count)
    |> Seq.map (fun (a, v) -> make_cell ~ranges ~prev a v)
    |> List.of_seq
  in
  { bound = Mem.next_addr st.mem; start; cells }

(** Typed value query: "42", "-3", "3.14", "true", "'c'", "0x1f" (pointer). *)
let parse_query q =
  let q = String.trim q in
  let n = String.length q in
  if q = "" then None
  else if q = "true" || q = "false" then
    Some (function RTVal.Bool b -> b = (q = "true") | _ -> false)
  else if n = 3 && q.[0] = '\'' && q.[2] = '\'' then
    Some (function RTVal.Char c -> c = q.[1] | _ -> false)
  else if n > 2 && q.[0] = '0' && q.[1] = 'x' then
    Option.map
      (fun a -> function RTVal.Addr x -> x = a | _ -> false)
      (int_of_string_opt q)
  else
    match int_of_string_opt q with
    | Some i -> Some (function RTVal.Int x -> x = i | _ -> false)
    | None -> (
        match float_of_string_opt q with
        | Some f when String.contains q '.' ->
            Some (function RTVal.Double x -> x = f | _ -> false)
        | _ -> None)

(** Scan the memory of log [log] for cells equal to the typed value [query].
    Stops after [limit] matches. [None] if the query cannot be parsed. *)
let mem_search t ~log ~query ~limit =
  match parse_query query with
  | None -> None
  | Some pred ->
      let st = state_at t log in
      let ranges = label_ranges st and prev = prev_mem t log in
      let rec go seq acc k =
        match seq () with
        | Seq.Nil -> (List.rev acc, false)
        | Seq.Cons ((a, v), rest) ->
            if not (pred v) then go rest acc k
            else if k >= limit then (List.rev acc, true)
            else go rest (make_cell ~ranges ~prev a v :: acc) (k + 1)
      in
      Some (go (Mem.to_seq_from 0 st.mem) [] 0)
