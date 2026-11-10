let () =
  let lexbuf = Lexing.from_channel stdin in
  let r, logs = MaoLang.run lexbuf in
  Fmt.pr "%a" MaoLang.DataStructure_pp.pp_logs logs;
  match r with
  | Ok _ -> ()
  | Error located_msg -> Fmt.epr "%a@." (Loc.pp Fmt.string) located_msg
