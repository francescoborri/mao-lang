let () =
  let lexbuf = Lexing.from_channel stdin in
  let pp_error = Loc.pp Fmt.string in
  match MaoLang.run lexbuf with
  | NotExecuted located_msg -> Fmt.epr "%a@." pp_error located_msg
  | Executed (r, logs) -> (
      Fmt.pr "%a" MaoLang.DataStructure_pp.pp_logs logs;
      match r with
      | Ok _ -> ()
      | Error located_msg -> Fmt.epr "%a@." pp_error located_msg)
