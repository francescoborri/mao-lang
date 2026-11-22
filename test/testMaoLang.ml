open TestCommon

let error_of ?max_depth src =
  match MaoLang.run ?max_depth (lexbuf src) with
  | NotExecuted e | Executed (Error e, _) -> e
  | Executed (Ok _, _) -> Alcotest.failf "%S: no error" src

let check_error ?max_depth ~prefix ~at src =
  let Loc.{ it = msg; at = { start_pos } } = error_of ?max_depth src in
  Alcotest.(check bool)
    (Fmt.str "%S starts with %S" msg prefix)
    true
    (String.starts_with ~prefix msg);
  Alcotest.(check (pair int int)) "position" at (start_pos.line, start_pos.col)

let executed src =
  match MaoLang.run (lexbuf src) with
  | Executed _ -> true
  | NotExecuted _ -> false

let tests =
  [
    test "errors of each stage" (fun () ->
        check_error ~prefix:"Errore lessicale: " ~at:(2, 9) "int x;\nint y = @;";
        check_error ~prefix:"Errore di sintassi" ~at:(1, 9) "int x = ;";
        check_error ~prefix:"Errore di tipo: " ~at:(1, 9) "int x = y;";
        check_error ~prefix:"Errore di esecuzione: " ~at:(4, 13)
          "int x = 1; // nota\n/* blocco\n   su due righe */\nint y = x / 0;");
    test "syntax errors name the rejected token and give a hint" (fun () ->
        List.iter
          (fun (src, at, expected) ->
            let Loc.{ it = msg; at = { start_pos } } = error_of src in
            Alcotest.(check string) src ("Errore di sintassi: " ^ expected) msg;
            Alcotest.(check (pair int int))
              (src ^ " position") at
              (start_pos.line, start_pos.col))
          [
            ( "int x = 1\nint y = 2;",
              (2, 1),
              "'int' inatteso. Forse manca ';' alla fine del comando, oppure \
               un operatore o una ',' tra due espressioni" );
            ( "int x; x = 1;",
              (1, 10),
              "'=' inatteso. Il simbolo '=' si usa solo per inizializzare una \
               variabile nella sua dichiarazione: per assegnare un valore si \
               usa ':=', per confrontare '=='" );
            ( "int x = true ? 1 : 2;",
              (1, 14),
              "'?' inatteso. L'espressione condizionale va scritta tra \
               parentesi: (condizione ? e1 : e2)" );
            ( "if (true) { skip; } else if (false) { skip; }",
              (1, 26),
              "'if' inatteso. Dopo 'else' ci vuole un blocco tra graffe: Mao \
               non ha 'else if', quindi scrivi else { if (...) { ... } }" );
            ( "while (true) { }",
              (1, 16),
              "'}' inatteso. Qui ci si aspetta un comando: un blocco non può \
               essere vuoto, se non deve fare nulla scrivi 'skip;'" );
            ( "while (true) { skip;",
              (1, 21),
              "fine del programma inattesa. Qui ci si aspetta un altro comando \
               oppure '}' per chiudere il blocco" );
            ("int x = 1 + ;", (1, 13), "';' inatteso");
            ( "char c = 'a' 'b';",
              (1, 14),
              "'b' inatteso. Forse manca ';' alla fine del comando, oppure un \
               operatore o una ',' tra due espressioni" );
          ]);
    test "only programs that pass the static checks are executed" (fun () ->
        List.iter
          (fun (src, expected) ->
            Alcotest.(check bool) src expected (executed src))
          [
            ("int x = @;", false);
            ("int x = ;", false);
            ("int x = true;", false);
            ("int x = 1 / 0;", true);
            ("int x = 1;", true);
          ]);
    test "logs before a runtime error are kept" (fun () ->
        let res, logs = run_traced "int x = 1; int y = x / 0;" in
        Alcotest.(check bool) "error" true (Result.is_error res);
        Alcotest.(check int) "logs" 1 (List.length logs));
    test "log limit" (fun () ->
        let res, logs =
          run_traced ~max_logs:10 "int n; while (true) { n := n + 1; }"
        in
        Alcotest.(check bool) "error" true (Result.is_error res);
        Alcotest.(check int) "logs" 10 (List.length logs));
    test "recursion limit keeps the trace" (fun () ->
        let src = "p(int n) { if (n > 0) { p(n - 1); } } p(100);" in
        let _, logs = run_traced ~max_depth:3 src in
        check_error ~max_depth:3 ~prefix:"Errore di esecuzione: " ~at:(1, 25)
          src;
        Alcotest.(check bool) "logs kept" true (logs <> []));
    test "parse ignores types, parse_type_check does not" (fun () ->
        let src = "int x = true;" in
        Alcotest.(check bool)
          "parse" true
          (Result.is_ok (MaoLang.parse (lexbuf src)));
        Alcotest.(check bool)
          "parse_type_check" true
          (Result.is_error (MaoLang.parse_type_check (lexbuf src))));
    test "messages are printed with their position" (fun () ->
        match MaoLang.parse_type_check (lexbuf "int x = y;") with
        | Error e ->
            Alcotest.(check string)
              "message"
              "Errore di tipo: 'y' non è stato dichiarato (riga 1, colonna 9)"
              (Fmt.str "%a" (Loc.pp Fmt.string) e)
        | Ok _ -> Alcotest.fail "no error");
  ]
