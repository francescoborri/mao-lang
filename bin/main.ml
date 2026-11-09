let () =
  let lexbuf = Lexing.from_channel stdin in
  let res, logs = MaoLang.run lexbuf in
  let logs = List.mapi (fun i log -> (i, log)) logs in
  let pp_log fmt log = Fmt.pf fmt "%s" log.MaoLang.Interpreter.event in
  let pp_log fmt (i, log) = Fmt.pf fmt "#%d: %a" (i + 1) (Loc.pp pp_log) log in
  Fmt.pr "%a%a"
    Fmt.(list ~sep:(any "@.") pp_log)
    logs
    (if List.is_empty logs then Fmt.nop else Fmt.cut)
    ();
  match res with
  | Ok _ -> Fmt.pr "Program executed successfully.@."
  | Error located_msg -> Fmt.pr "%a@." (Loc.pp Fmt.string) located_msg
