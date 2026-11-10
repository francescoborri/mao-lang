open MaoLang.Ast_pp
open MaoLang.DataStructure_pp
open Js_of_ocaml
open Fmt

let pp_loc_str = Loc.pp Fmt.string

let fmt src =
  let lexbuf = Lexing.from_string (Js.to_string src) in
  let res = MaoLang.type_check lexbuf in
  let ok, output =
    match res with
    | Ok ast -> (true, Fmt.to_to_string pp_command ast)
    | Error located_msg -> (false, Fmt.to_to_string pp_loc_str located_msg)
  in
  Js.Unsafe.obj
    [|
      ("ok", Js.Unsafe.inject (Js.bool ok));
      ("output", Js.Unsafe.inject (Js.string output));
    |]

let run src =
  let lexbuf = Lexing.from_string (Js.to_string src) in
  let res, logs = MaoLang.run lexbuf in
  let ok, output =
    match res with
    | Ok _ -> (true, Fmt.str "%a" pp_logs_short logs)
    | Error msg -> (false, Fmt.str "%a%a" pp_logs_short logs pp_loc_str msg)
  in
  Js.Unsafe.obj
    [|
      ("ok", Js.Unsafe.inject (Js.bool ok));
      ("output", Js.Unsafe.inject (Js.string output));
    |]

let () =
  Js.Unsafe.set Js.Unsafe.global (Js.string "runProgram") (Js.wrap_callback run);
  Js.Unsafe.set Js.Unsafe.global
    (Js.string "formatProgram")
    (Js.wrap_callback fmt)
