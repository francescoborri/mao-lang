open Js_of_ocaml
open Fmt

let run_program (src : Js.js_string Js.t) =
  let lexbuf = Lexing.from_string (Js.to_string src) in
  let res, logs = MaoLang.run lexbuf in
  let logs =
    List.map (fun Loc.{ it = log } -> log.MaoLang.Interpreter.event) logs
  in
  let logs_str = Fmt.to_to_string (Fmt.list ~sep:(any "\n") string) logs in
  let ok, output =
    match res with
    | Ok _ -> (true, Fmt.str "%s\nProgram executed successfully." logs_str)
    | Error msg -> (false, Fmt.str "%s\n%a" logs_str (Loc.pp Fmt.string) msg)
  in
  Js.Unsafe.obj
    [|
      ("ok", Js.Unsafe.inject (Js.bool ok));
      ("output", Js.Unsafe.inject (Js.string output));
    |]

let () =
  Js.Unsafe.set Js.Unsafe.global (Js.string "runProgram")
    (Js.wrap_callback run_program)
