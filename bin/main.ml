let () =
  let lexbuf = Lexing.from_channel stdin in
  let res, logs = MaoLang.run lexbuf in
  Fmt.pr "Logs: %a@." Fmt.(list ~sep:(any "@.") string) logs;
  match res with
  | Ok _ -> Fmt.pr "Program executed successfully.@."
  | Error located_msg -> Fmt.pr "%a@." (Loc.pp Fmt.string) located_msg
