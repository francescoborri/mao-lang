(* JS API of the interpreter, registered as the global object [Mao] and called
   by web/worker.js. The trace of the last run stays on the OCaml side, and
   each function returns a single page of it:

     run(source, maxLogs)                 -> { ok, total, error, kinds, maxDepth }
     format(source)                       -> { ok, output } | { ok, error }
     filter(kinds, query)                 -> { total }
     logs(offset, limit)                  -> page of entries
     position(index)                      -> position in the filtered trace
     env(index, query, offset, limit)     -> page of variables
     memory(index, query, offset, limit)  -> page of memory cells

   Functions never raise: failures are returned as { fatal }. *)

open Js_of_ocaml
open Utils
module Trace = MaoLang.Trace

module OfJs = struct
  let int n = int_of_float (Js.float_of_number n)
  let str = Js.to_string
  let list arr = Js.to_array arr |> Array.to_list
end

module ToJs = struct
  let obj fields = Js.Unsafe.obj (Array.of_list fields)
  let str s = Js.Unsafe.inject (Js.string s)
  let bool b = Js.Unsafe.inject (Js.bool b)
  let int i = Js.Unsafe.inject (Js.number_of_float (float_of_int i))
  let null = Js.Unsafe.inject Js.null
  let option f = function Some x -> f x | None -> null
  let list f xs = Js.Unsafe.inject (Js.array (Array.of_list (List.map f xs)))

  let error Loc.{ it = msg; at = { start_pos; end_pos } } =
    obj
      [
        ("message", str msg);
        ("line", int start_pos.line);
        ("col", int start_pos.col);
        ("start", int start_pos.offset);
        ("end", int end_pos.offset);
      ]

  let kind_counts counts =
    counts
    |> List.map (fun (kind, n) -> (Trace.Kind.to_string kind, int n))
    |> obj

  let page f Trace.{ items; offset; total } =
    obj
      [ ("items", list f items); ("offset", int offset); ("total", int total) ]

  let entry Trace.Entry.{ index; kind; name; descr; depth; loc } =
    obj
      [
        ("i", int index);
        ("kind", str (Trace.Kind.to_string kind));
        ("name", str name);
        ("descr", str descr);
        ("depth", int depth);
        ("line", int loc.start_pos.line);
        ("col", int loc.start_pos.col);
        ("endLine", int loc.end_pos.line);
        ("endCol", int loc.end_pos.col);
        ("start", int loc.start_pos.offset);
        ("end", int loc.end_pos.offset);
      ]

  let var Trace.Var.{ name; ty; value; array; addr; changed } =
    obj
      [
        ("name", str name);
        ("type", str ty);
        ("value", str value);
        ("array", option (fst >> int) array);
        ("preview", option (snd >> str) array);
        ("addr", option int addr);
        ("changed", bool changed);
      ]

  let cell Trace.Cell.{ addr; value; ty; owner; changed } =
    obj
      [
        ("addr", int addr);
        ("value", str value);
        ("type", str ty);
        ("owner", str owner);
        ("changed", bool changed);
      ]
end

let trace = ref None

let guard f =
  try f () with e -> ToJs.obj [ ("fatal", ToJs.str (Printexc.to_string e)) ]

let with_trace f =
  guard (fun () ->
      match !trace with
      | Some t -> f t
      | None ->
          failwith "nessuna traccia disponibile: esegui prima un programma")

let lexbuf_of src = Lexing.from_string (OfJs.str src)

let run src max_logs =
  guard (fun () ->
      let max_logs = OfJs.int max_logs in
      let max_logs = if max_logs > 0 then Some max_logs else None in
      let res, logs = MaoLang.run ?max_logs (lexbuf_of src) in
      let t = Trace.of_logs logs in
      trace := Some t;
      ToJs.obj
        [
          ("ok", ToJs.bool (Result.is_ok res));
          ("total", ToJs.int (Trace.length t));
          ("error", Result.fold ~ok:(Fun.const ToJs.null) ~error:ToJs.error res);
          ("kinds", ToJs.kind_counts (Trace.kind_counts t));
          ("maxDepth", ToJs.int (Trace.max_depth t));
        ])

(* It only parses, so that ill-typed programs can still be formatted *)
let format src =
  guard (fun () ->
      match MaoLang.parse (lexbuf_of src) with
      | Ok ast ->
          let output = Fmt.str "%a@." MaoLang.Ast_pp.pp_command ast in
          ToJs.obj [ ("ok", ToJs.bool true); ("output", ToJs.str output) ]
      | Error e -> ToJs.obj [ ("ok", ToJs.bool false); ("error", ToJs.error e) ])

let filter kinds query =
  with_trace (fun t ->
      let kinds =
        OfJs.list kinds |> List.filter_map (OfJs.str >> Trace.Kind.of_string)
      in
      let t = Trace.filter t ~kinds ~query:(OfJs.str query) in
      trace := Some t;
      ToJs.obj [ ("total", ToJs.int (Trace.view_length t)) ])

let logs offset limit =
  with_trace (fun t ->
      Trace.entries t ~offset:(OfJs.int offset) ~limit:(OfJs.int limit)
      |> ToJs.page ToJs.entry)

let position index =
  with_trace (fun t -> ToJs.int (Trace.position t (OfJs.int index)))

let env index query offset limit =
  with_trace (fun t ->
      Trace.vars t (OfJs.int index) ~query:(OfJs.str query)
        ~offset:(OfJs.int offset) ~limit:(OfJs.int limit)
      |> ToJs.page ToJs.var)

let memory index query offset limit =
  with_trace (fun t ->
      Trace.memory t (OfJs.int index) ~query:(OfJs.str query)
        ~offset:(OfJs.int offset) ~limit:(OfJs.int limit)
      |> ToJs.page ToJs.cell)

let () =
  let fn f = Js.Unsafe.inject (Js.wrap_callback f) in
  Js.Unsafe.set Js.Unsafe.global (Js.string "Mao")
    (ToJs.obj
       [
         ("run", fn run);
         ("format", fn format);
         ("filter", fn filter);
         ("logs", fn logs);
         ("position", fn position);
         ("env", fn env);
         ("memory", fn memory);
       ])
