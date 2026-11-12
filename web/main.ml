(* JS-facing API of the interpreter (see web/worker.js for the RPC layer).

   Nothing big ever crosses the OCaml/JS boundary: [run] keeps the whole trace
   on the OCaml side (see MaoTrace) and every other function returns one small
   window of it (a page of logs, a page of memory, the environment of one log).

   All functions are exposed on the global object [Mao]. They never raise: an
   internal failure is reported as { fatal : string }. *)

open Js_of_ocaml
module T = MaoLang.Trace

(* ---- tiny helpers to build plain JS values ------------------------------ *)

let js_obj fields = Js.Unsafe.obj (Array.of_list fields)
let js_str x = Js.Unsafe.inject (Js.string x)
let js_bool x = Js.Unsafe.inject (Js.bool x)
let js_int x = Js.Unsafe.inject (Js.number_of_float (float_of_int x))
let js_null = Js.Unsafe.inject Js.null
let js_arr l = Js.Unsafe.inject (Js.array (Array.of_list l))
let to_int (x : Js.number Js.t) = int_of_float (Js.float_of_number x)

let guard f =
  try f () with e -> js_obj [ ("fatal", js_str (Printexc.to_string e)) ]

(* ---- session ------------------------------------------------------------ *)

let current : T.t option ref = ref None

let session () =
  match !current with
  | Some t -> t
  | None -> failwith "no trace available: run a program first"

(* ---- conversions -------------------------------------------------------- *)

let error_obj Loc.{ it; at } =
  js_obj
    [
      ("message", js_str it);
      ("line", js_int at.start_pos.line);
      ("col", js_int at.start_pos.col);
      ("start", js_int at.start_pos.offset);
      ("end", js_int at.end_pos.offset);
    ]

let row_obj (r : T.row) =
  let open Loc in
  js_obj
    [
      ("i", js_int r.index);
      ("kind", js_str r.kind);
      ("name", js_str r.name);
      ("descr", js_str r.descr);
      ("line", js_int r.loc.start_pos.line);
      ("col", js_int r.loc.start_pos.col);
      ("start", js_int r.loc.start_pos.offset);
      ("end", js_int r.loc.end_pos.offset);
      ("endLine", js_int r.loc.end_pos.line);
      ("endCol", js_int r.loc.end_pos.col);
      ("depth", js_int r.depth);
      ("call", js_int r.call);
      ("callEnd", js_int r.call_end);
      ("outEnd", js_int r.out_end);
    ]

let env_obj (r : T.env_row) =
  js_obj
    [
      ("name", js_str r.var);
      ("kind", js_str r.vkind);
      ("addr", js_int r.addr);
      ("value", js_str r.value);
      ("type", js_str r.ty);
      ("detail", js_str r.detail);
    ]

let cell_obj (c : T.cell) =
  js_obj
    [
      ("addr", js_int c.caddr);
      ("value", js_str c.cvalue);
      ("type", js_str c.cty);
      ("changed", js_bool c.changed);
      ("label", js_str c.label);
    ]

(* ---- API ---------------------------------------------------------------- *)

(** run(source, maxLogs) -> { ok, total, error|null, kinds, maxDepth }
    [maxLogs] <= 0 means unlimited. *)
let run src max_logs =
  guard (fun () ->
      current := None;
      let max_logs =
        let m = to_int max_logs in
        if m <= 0 then None else Some m
      in
      let res, logs =
        MaoLang.run ?max_logs (Lexing.from_string (Js.to_string src))
      in
      let t = T.of_logs logs in
      current := Some t;
      js_obj
        [
          ("ok", js_bool (Result.is_ok res));
          ("total", js_int (T.length t));
          ("error", match res with Ok _ -> js_null | Error e -> error_obj e);
          ( "kinds",
            js_obj (List.map (fun (k, c) -> (k, js_int c)) (T.kind_counts t)) );
          ("maxDepth", js_int (T.max_depth t));
        ])

(** format(source) -> { ok, output } | { ok = false, error }.
    Only parses: ill-typed programs can still be formatted. *)
let format src =
  guard (fun () ->
      match MaoLang.parse (Lexing.from_string (Js.to_string src)) with
      | Ok ast ->
          js_obj
            [
              ("ok", js_bool true);
              ( "output",
                js_str (Fmt.to_to_string MaoLang.Ast_pp.pp_command ast ^ "\n")
              );
            ]
      | Error e -> js_obj [ ("ok", js_bool false); ("error", error_obj e) ])

(** filter(kinds, text) -> { count }: restrict the log view. *)
let filter kinds text =
  guard (fun () ->
      let t = session () in
      let kinds =
        Js.to_array kinds |> Array.map Js.to_string |> Array.to_list
      in
      let count = T.set_filter t ~kinds ~text:(Js.to_string text) in
      js_obj [ ("count", js_int count) ])

(** logs(offset, limit) -> { total, offset, rows } over the current view. *)
let logs offset limit =
  guard (fun () ->
      let t = session () in
      let offset = to_int offset in
      let rows = T.page t ~offset ~limit:(to_int limit) in
      js_obj
        [
          ("total", js_int (T.view_length t));
          ("offset", js_int offset);
          ("rows", js_arr (List.map row_obj rows));
        ])

(** row(index) -> { row, pos }: log [index] and its position in the view. *)
let row index =
  guard (fun () ->
      let t = session () in
      let i = to_int index in
      if i < 0 || i >= T.length t then failwith "index out of range"
      else
        js_obj [ ("row", row_obj (T.row t i)); ("pos", js_int (T.locate t i)) ])

(** env(index) -> { rows }: the environment after log [index]. *)
let env index =
  guard (fun () ->
      let t = session () in
      js_obj
        [ ("rows", js_arr (List.map env_obj (T.env_rows t (to_int index)))) ])

(** memory(index, start, count) -> { bound, start, cells } *)
let memory index start count =
  guard (fun () ->
      let t = session () in
      let p =
        T.mem_page t ~log:(to_int index) ~start:(to_int start)
          ~count:(to_int count)
      in
      js_obj
        [
          ("bound", js_int p.bound);
          ("start", js_int p.start);
          ("cells", js_arr (List.map cell_obj p.cells));
        ])

(** memSearch(index, query, limit) -> { ok, cells, truncated } *)
let mem_search index query limit =
  guard (fun () ->
      let t = session () in
      match
        T.mem_search t ~log:(to_int index) ~query:(Js.to_string query)
          ~limit:(to_int limit)
      with
      | None ->
          js_obj
            [
              ("ok", js_bool false);
              ( "error",
                js_str
                  "Not a value. Try 42, -3, 3.14, true, 'c' or 0x1f (pointer)."
              );
            ]
      | Some (cells, truncated) ->
          js_obj
            [
              ("ok", js_bool true);
              ("cells", js_arr (List.map cell_obj cells));
              ("truncated", js_bool truncated);
            ])

let () =
  let fn f = Js.Unsafe.inject (Js.wrap_callback f) in
  let api =
    js_obj
      [
        ("run", fn run);
        ("format", fn format);
        ("filter", fn filter);
        ("logs", fn logs);
        ("row", fn row);
        ("env", fn env);
        ("memory", fn memory);
        ("memSearch", fn mem_search);
      ]
  in
  Js.Unsafe.set Js.Unsafe.global (Js.string "Mao") api
