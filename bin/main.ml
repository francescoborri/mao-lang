let pp_table fmt logs =
  if logs = [] then Fmt.pf fmt ""
  else
    let headers = [ "index"; "event"; "descr"; "from"; "to" ] in
    let cells =
      List.mapi
        (fun i Loc.{ it = MaoLang.Interpreter.{ event; descr }; at = loc } ->
          [
            string_of_int i;
            event;
            descr;
            Fmt.to_to_string Loc.pp_loc loc;
            Fmt.to_to_string Loc.pp_end_loc loc;
          ])
        logs
    in
    let columns =
      List.init (List.length headers) (fun i ->
          List.map (fun row -> List.nth row i) cells)
    in
    let widths =
      List.map2
        (fun header column ->
          List.fold_left
            (fun width cell -> max width (String.length cell))
            (String.length header) column)
        headers columns
    in
    let print_row values =
      List.iter2
        (fun width value -> Fmt.pf fmt "| %-*s " width value)
        widths values;
      Fmt.pf fmt "|@."
    in
    print_row headers;
    List.iter
      (fun width -> Fmt.pf fmt "|%s" (String.make (width + 2) '-'))
      widths;
    Fmt.pf fmt "|@.";
    List.iter print_row cells

let () =
  let lexbuf = Lexing.from_channel stdin in
  let res, logs = MaoLang.run lexbuf in
  Fmt.pr "%a" pp_table logs;
  match res with
  | Ok _ -> ()
  | Error located_msg -> Fmt.epr "%a@." (Loc.pp Fmt.string) located_msg
