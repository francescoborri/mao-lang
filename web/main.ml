(* The only OCaml file that knows about JavaScript.
   It runs inside the Web Worker (web/worker.js) and registers one global
   function there:

     runProgram(source: string) -> { ok: boolean, output: string }
*)
open Js_of_ocaml
open MaoLang
open Fmt

let run_program (src : Js.js_string Js.t) =
  let lexbuf = Lexing.from_string (Js.to_string src) in
  let ok, output =
    try
      let ast = MaoParser.program MaoLexer.read lexbuf in
      match MaoTypeChecker.type_check_program ast with
      | Ok () -> (true, Fmt.str "%a@." MaoAst_pp.pp_command ast)
      | Error msg -> (false, Fmt.str "Type error: %s\n" msg)
    with
    | MaoLexer.LexerError msg -> (false, Fmt.str "[Lexer error] %s\n" msg)
    | MaoParser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        ( false,
          Fmt.str "Parse error: line %d, column %d\n" pos.Lexing.pos_lnum
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1) )
  in
  Js.Unsafe.obj
    [|
      ("ok", Js.Unsafe.inject (Js.bool ok));
      ("output", Js.Unsafe.inject (Js.string output));
    |]

let () =
  Js.Unsafe.set Js.Unsafe.global (Js.string "runProgram")
    (Js.wrap_callback run_program)
