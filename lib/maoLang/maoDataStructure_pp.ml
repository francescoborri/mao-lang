open MaoAst
open MaoAst_pp
open MaoDataStructure

let pp_event_name fmt = function
  | EDeclare _ -> Fmt.pf fmt "decl"
  | EDeclares _ -> Fmt.pf fmt "decls"
  | EAssign _ -> Fmt.pf fmt "assign"
  | EAssigns _ -> Fmt.pf fmt "assigns"
  | EArrayStore (_, _, _, addr) -> Fmt.pf fmt "store to %a" IntAddr.pp addr
  | EIfThenElse _ -> Fmt.pf fmt "if"
  | EWhile _ -> Fmt.pf fmt "while"
  | EFunc _ -> Fmt.pf fmt "decl func"
  | EProc _ -> Fmt.pf fmt "decl proc"
  | EBeginCall (_, call_id) -> Fmt.pf fmt "begin call #%d" call_id
  | EEndCall (_, call_id, _) -> Fmt.pf fmt "end call #%d" call_id
  | EBeginInvoke (_, call_id) -> Fmt.pf fmt "begin invoke #%d" call_id
  | EEndInvoke (_, call_id) -> Fmt.pf fmt "end invoke #%d" call_id

let pp_event_descr fmt = function
  | EDeclare (t, id, v) | EDeclares (t, id, v) ->
      Fmt.pf fmt "%a %s = %a" pp_mao_type t id RTVal.pp v
  | EAssign (id, v) | EAssigns (id, v) -> Fmt.pf fmt "%s := %a" id RTVal.pp v
  | EArrayStore (id, idx, v, _) -> Fmt.pf fmt "%s[%d] := %a" id idx RTVal.pp v
  | EIfThenElse b | EWhile b -> Fmt.bool fmt b
  | EFunc id | EProc id -> Fmt.string fmt id
  | EBeginCall (id, _)
  | EEndCall (id, _, _)
  | EBeginInvoke (id, _)
  | EEndInvoke (id, _) ->
      Fmt.pf fmt "%s(...)" id

let pp_table ?(with_index = false) named_pps fmt logs =
  if logs = [] then Fmt.pf fmt ""
  else
    let headers = List.map fst named_pps in
    let headers = if with_index then "index" :: headers else headers in
    let pps = List.map snd named_pps in
    let cells =
      List.mapi
        (fun i log ->
          let cells = List.map (fun pp -> Fmt.to_to_string pp log) pps in
          if with_index then string_of_int i :: cells else cells)
        logs
    in
    let columns =
      List.init (List.length headers) (fun i ->
          List.map (fun row -> List.nth row i) cells)
    in
    let widths =
      List.map2
        (fun header column ->
          List.fold_left
            (fun width cell -> max width (String.length cell))
            (String.length header) column)
        headers columns
    in
    let print_row values =
      List.iter2
        (fun width value -> Fmt.pf fmt "| %-*s " width value)
        widths values;
      Fmt.pf fmt "|@."
    in
    print_row headers;
    List.iter
      (fun width -> Fmt.pf fmt "|%s" (String.make (width + 2) '-'))
      widths;
    Fmt.pf fmt "|@.";
    List.iter print_row cells

let pp_logs =
  pp_table ~with_index:true
    [
      ("event", Fmt.using (fun Loc.{ it } -> it.event) pp_event_name);
      ("descr", Fmt.using (fun Loc.{ it } -> it.event) pp_event_descr);
      ("from", Fmt.using (fun Loc.{ at } -> at) Loc.pp_loc);
      ("to", Fmt.using (fun Loc.{ at } -> at) Loc.pp_end_loc);
    ]

let pp_logs_short =
  pp_table
    [
      ("event", Fmt.using (fun Loc.{ it } -> it.event) pp_event_name);
      ("descr", Fmt.using (fun Loc.{ it } -> it.event) pp_event_descr);
    ]
