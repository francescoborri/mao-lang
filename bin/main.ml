open MaoLang

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = MaoParser.program MaoLexer.read lexbuf in
    match MaoTypeChecker.type_check_program ast with
    | Ok () -> Fmt.pr "%a@." MaoAst_pp.pp_command ast
    | Error msg ->
        Fmt.epr "[Type error] %s\n" msg;
        exit 1
  with
  | MaoLexer.LexerError msg ->
      Fmt.epr "[Lexer error] %s\n" msg;
      exit 1
  | MaoParser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Fmt.epr "[Parse error] line %d, column %d\n" pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
      exit 1
