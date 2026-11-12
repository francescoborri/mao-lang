module Ast = MaoAst
module Ast_pp = MaoAst_pp
module DataStructure = MaoDataStructure
module DataStructure_pp = MaoDataStructure_pp
module Lexer = MaoLexer
module Parser = MaoParser
module TypeChecker = MaoTypeChecker
module Interpreter = MaoInterpreter
module Trace = MaoTrace

let parse lexbuf =
  try Result.ok (MaoParser.program MaoLexer.read lexbuf) with
  | MaoLexer.Error msg ->
      msg |> Loc.map_item (Fmt.str "Lexer error: %s") |> Result.error
  | MaoParser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      "Parse error"
      |> Loc.with_positions ~start_pos:pos ~end_pos:pos
      |> Result.error

let type_check lexbuf =
  let open Result.Syntax in
  let* ast = parse lexbuf in
  match MaoTypeChecker.type_check_program ast with
  | Ok _ -> Result.ok ast
  | Error msg -> msg |> Loc.map_item (Fmt.str "Type error: %s") |> Result.error

let run ?max_logs lexbuf =
  let module WRM = WriterResultMonad in
  let open WRM.Syntax in
  let m =
    let* c_ast =
      try
        let ast = MaoParser.program MaoLexer.read lexbuf in
        match MaoTypeChecker.type_check_program ast with
        | Ok c_ast -> WRM.ok c_ast
        | Error msg ->
            msg |> Loc.map_item (Fmt.str "Type error: %s") |> WRM.error
      with
      | MaoLexer.Error msg ->
          msg |> Loc.map_item (Fmt.str "Lexer error: %s") |> WRM.error
      | MaoParser.Error ->
          let pos = lexbuf.Lexing.lex_curr_p in
          "Parse error"
          |> Loc.with_positions ~start_pos:pos ~end_pos:pos
          |> WRM.error
    in
    MaoInterpreter.run_program ?max_logs c_ast
    =&> Loc.map_item (Fmt.str "Runtime error: %s")
  in
  try WRM.run m
  with Utils.Unreachable msg ->
    msg
    |> Fmt.str "Unreachable code reached: %s"
    |> Loc.with_loc ~loc:Loc.dummy_loc
    |> WRM.error |> WRM.run
