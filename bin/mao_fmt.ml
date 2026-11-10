let () =
  let lexbuf = Lexing.from_channel stdin in
  let r = MaoLang.type_check lexbuf in
  match r with
  | Ok ast -> Fmt.pr "%a@." MaoLang.Ast_pp.pp_command ast
  | Error located_msg -> Fmt.epr "%a@." (Loc.pp Fmt.string) located_msg
