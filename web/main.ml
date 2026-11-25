open Js_of_ocaml
open Utils
module Trace = MaoLang.Trace

module OfJs = struct
  let int n = Js.float_of_number n |> int_of_float
  let str = Js.to_string
  let bool = Js.to_bool
  let list arr = Js.to_array arr |> Array.to_list
  let limit n = match int n with n when n > 0 -> Some n | _ -> None
end

module ToJs = struct
  let obj fields = Js.Unsafe.obj (Array.of_list fields)
  let str s = Js.Unsafe.inject (Js.string s)
  let bool b = Js.Unsafe.inject (Js.bool b)
  let int i = Js.Unsafe.inject (Js.number_of_float (float_of_int i))
  let int64 i = Js.Unsafe.inject (Js.number_of_float (Int64.to_float i))
  let null = Js.Unsafe.inject Js.null
  let option f x = Option.fold ~none:null ~some:f x
  let list f xs = List.map f xs |> Array.of_list |> Js.array |> Js.Unsafe.inject
  let fatal msg = obj [ ("fatal", str msg) ]

  let loc Loc.{ start_pos; end_pos } =
    [
      ("line", int start_pos.line);
      ("col", int start_pos.col);
      ("endLine", int end_pos.line);
      ("endCol", int end_pos.col);
      ("start", int start_pos.offset);
      ("end", int end_pos.offset);
    ]

  let error Loc.{ it = msg; at } =
    obj (("message", str msg) :: (if Loc.is_dummy at then [] else loc at))

  let kind_counts counts =
    counts
    |> List.map (fun (kind, n) -> (Trace.Kind.to_string kind, int n))
    |> obj

  let page f Trace.Page.{ items; offset; total } =
    obj
      [ ("items", list f items); ("offset", int offset); ("total", int total) ]

  let entry Trace.Entry.{ index; kind; name; descr; depth; loc = at } =
    obj
      ([
         ("i", int index);
         ("kind", str (Trace.Kind.to_string kind));
         ("name", str name);
         ("descr", str descr);
         ("depth", int depth);
       ]
      @ loc at)

  let var Trace.Var.{ name; ty; value; array; addr; changed } =
    obj
      [
        ("name", str name);
        ("type", str ty);
        ("value", str value);
        ("array", option (fst >> int64) array);
        ("preview", option (snd >> str) array);
        ("addr", option int64 addr);
        ("changed", bool changed);
      ]

  let cell Trace.Cell.{ addr; value; ty; owner; changed } =
    obj
      [
        ("addr", int64 addr);
        ("value", str value);
        ("type", str ty);
        ("owner", str owner);
        ("changed", bool changed);
      ]
end

(* the trace of the last run (and its current filter) lives between the
   worker's calls, so that paging through it never re-runs the program *)
let trace = ref None
let guard f = try f () with e -> ToJs.fatal (Printexc.to_string e)

let with_trace f =
  guard (fun () ->
      match !trace with
      | Some t -> f t
      | None ->
          ToJs.fatal "nessuna traccia disponibile: esegui prima un programma")

let lexbuf_of s = Lexing.from_string (OfJs.str s)

let paged query offset limit =
  query ~offset:(OfJs.int offset) ~limit:(OfJs.int limit)

let run src max_logs max_depth extra =
  guard (fun () ->
      let max_logs, max_depth = (OfJs.limit max_logs, OfJs.limit max_depth) in
      let run = if OfJs.bool extra then MaoLang.run_extra else MaoLang.run in
      let executed, res, logs =
        match run ?max_logs ?max_depth (lexbuf_of src) with
        | Executed (res, logs) -> (true, Result.map ignore res, logs)
        | NotExecuted e -> (false, Error e, [])
      in
      let t = Trace.of_logs logs in
      trace := Some t;
      ToJs.obj
        [
          ("ok", ToJs.bool (Result.is_ok res));
          ("executed", ToJs.bool executed);
          ("total", ToJs.int (Trace.length t));
          ("error", Result.fold ~ok:(Fun.const ToJs.null) ~error:ToJs.error res);
          ("kinds", ToJs.kind_counts (Trace.kind_counts t));
          ("maxDepth", ToJs.int (Trace.max_depth t));
        ])

(* the extended parser expands the abbreviations, and the expansion of x++
   uses a name the lexer rejects, so a program that needs the extended parser
   is not formatted: the error points to the first abbreviation *)
let parse_for_format src extra =
  match MaoLang.parse (lexbuf_of src) with
  | Error e when OfJs.bool extra ->
      MaoLang.parse_extra (lexbuf_of src)
      |> Result.fold ~error:Result.error ~ok:(fun _ ->
          e
          |> Loc.map_item
               (Fun.const
                  "Il formattatore non supporta ancora le abbreviazioni della \
                   versione estesa (++, --, +=, -=, *=, /=)")
          |> Result.error)
  | parsed -> parsed

let format src extra =
  guard (fun () ->
      match parse_for_format src extra with
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
      paged (Trace.entries t) offset limit |> ToJs.page ToJs.entry)

let position index =
  with_trace (fun t -> ToJs.int (Trace.position t (OfJs.int index)))

let env index query offset limit =
  with_trace (fun t ->
      paged (Trace.vars t (OfJs.int index) ~query:(OfJs.str query)) offset limit
      |> ToJs.page ToJs.var)

let memory index query offset limit =
  with_trace (fun t ->
      paged
        (Trace.memory t (OfJs.int index) ~query:(OfJs.str query))
        offset limit
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
