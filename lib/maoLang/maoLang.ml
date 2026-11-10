module Ast = MaoAst
module Ast_pp = MaoAst_pp
module DataStructure = MaoDataStructure
module DataStructure_pp = MaoDataStructure_pp
module Lexer = MaoLexer
module Parser = MaoParser
module TypeChecker = MaoTypeChecker
module Interpreter = MaoInterpreter

let type_check lexbuf =
  let open Result.Syntax in
  try
    let ast = MaoParser.program MaoLexer.read lexbuf in
    match MaoTypeChecker.type_check_program ast with
    | Ok _ -> Result.ok ast
    | Error msg ->
        msg |> Loc.map_item (Fmt.str "Type error: %s") |> Result.error
  with
  | MaoLexer.Error msg ->
      msg |> Loc.map_item (Fmt.str "Lexer error: %s") |> Result.error
  | MaoParser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      "Parse error"
      |> Loc.with_positions ~start_pos:pos ~end_pos:pos
      |> Result.error

let run lexbuf =
  let module WRM = WriterResultMonad in
  let open WRM.Syntax in
  WRM.run
  @@
  let* c_ast =
    try
      let ast = MaoParser.program MaoLexer.read lexbuf in
      match MaoTypeChecker.type_check_program ast with
      | Ok c_ast -> WRM.ok c_ast
      | Error msg -> msg |> Loc.map_item (Fmt.str "Type error: %s") |> WRM.error
    with
    | MaoLexer.Error msg ->
        msg |> Loc.map_item (Fmt.str "Lexer error: %s") |> WRM.error
    | MaoParser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        "Parse error"
        |> Loc.with_positions ~start_pos:pos ~end_pos:pos
        |> WRM.error
  in
  try
    MaoInterpreter.run_program c_ast
    =&> Loc.map_item (Fmt.str "Runtime error: %s")
  with Utils.Unreachable msg ->
    msg
    |> Fmt.str "Unreachable code reached: %s"
    |> Loc.with_loc ~loc:Loc.dummy_loc
    |> WRM.error
