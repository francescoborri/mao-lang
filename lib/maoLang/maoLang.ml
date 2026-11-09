module Ast = MaoAst
module Ast_pp = MaoAst_pp
module Lexer = MaoLexer
module Parser = MaoParser
module TypeChecker = MaoTypeChecker
module Interpreter = MaoInterpreter

type log = Interpreter.log

let run lexbuf =
  let module WRM = WriterResultMonad in
  let open WRM.Syntax in
  let* ast =
    try
      let ast = MaoParser.program MaoLexer.read lexbuf in
      match MaoTypeChecker.type_check_program ast with
      | Ok () -> WRM.ok ast
      | Error located_msg ->
          located_msg |> Loc.map_item (Fmt.str "Type error: %s") |> WRM.error
    with
    | MaoLexer.Error located_msg ->
        located_msg |> Loc.map_item (Fmt.str "Lexer error: %s") |> WRM.error
    | MaoParser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        "Parse error"
        |> Loc.with_positions ~start_pos:pos ~end_pos:pos
        |> WRM.error
  in
  let res, logs = MaoInterpreter.run_program ast in
  let res = Result.map_error (Loc.map_item (Fmt.str "Runtime error: %s")) res in
  (res, logs)
