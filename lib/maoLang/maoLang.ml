module Ast = MaoAst
module Ast_pp = MaoAst_pp
module Lexer = MaoLexer
module Parser = MaoParser
module TypeChecker = MaoTypeChecker
module Interpreter = MaoInterpreter

let run lexbuf =
  let module RLM = ResultLogMonad in
  let open RLM.Syntax in
  let* ast =
    try
      let ast = MaoParser.program MaoLexer.read lexbuf in
      match MaoTypeChecker.type_check_program ast with
      | Ok () -> RLM.ok ast
      | Error located_msg ->
          located_msg |> Loc.map_item (Fmt.str "Type error: %s") |> RLM.error
    with
    | MaoLexer.Error located_msg ->
        located_msg |> Loc.map_item (Fmt.str "Lexer error: %s") |> RLM.error
    | MaoParser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        "Parse error"
        |> Loc.with_positions ~start_pos:pos ~end_pos:pos
        |> RLM.error
  in
  let res, logs = MaoInterpreter.run_program ast in
  let res = Result.map_error (Loc.map_item (Fmt.str "Runtime error: %s")) res in
  (res, logs)
