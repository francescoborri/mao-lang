open MaoLang
open TestCommon

let format src =
  match parse_extra (lexbuf src) with
  | Ok ast -> Fmt.str "%a" Ast_pp.pp_command ast
  | Error _ -> Alcotest.failf "%S: parse error" src

let parses_as src expected = Alcotest.(check string) src expected (format src)

let rejects parse src =
  Alcotest.(check bool) src true (Result.is_error (parse (lexbuf src)))

let error_at src =
  match run_extra (lexbuf src) with
  | NotExecuted Loc.{ it = msg; at = { start_pos } }
  | Executed (Error Loc.{ it = msg; at = { start_pos } }, _) ->
      (msg, (start_pos.line, start_pos.col))
  | Executed (Ok _, _) -> Alcotest.failf "%S: no error" src

let tests =
  [
    test "increments and decrements" (fun () ->
        check_values ~extra:true
          "int x = 5; int a = x++; int b = x; int c = ++x; int d = x--; int e \
           = --x;"
          [
            ("a", int 5); ("b", int 6); ("c", int 7); ("d", int 7); ("e", int 5);
          ];
        check_values ~extra:true
          "double x = 1.5; double a = x++; double b = ++x; double c = x--; \
           double d = --x;"
          [
            ("a", double 1.5);
            ("b", double 3.5);
            ("c", double 3.5);
            ("d", double 1.5);
          ];
        check_values ~extra:true
          "int x = 0; x++; x++; x--; double d = 0.0; d++; d--; d--;"
          [ ("x", int 1); ("d", double (-1.0)) ]);
    test "compound assignments" (fun () ->
        check_values ~extra:true "int x = 1; x += 10; x -= 2; x *= 3; x /= 4;"
          [ ("x", int 6) ];
        check_values ~extra:true "double d = 1.0; d += 0.5; d *= 2.0; d /= 4.0;"
          [ ("d", double 0.75) ];
        check_values ~extra:true "int x = 2; x *= 1 + 2;" [ ("x", int 6) ]);
    test "abbreviations nest and evaluate left to right" (fun () ->
        check_values ~extra:true "int x = 0; int y = 0; x += y++; x += ++y;"
          [ ("x", int 2); ("y", int 2) ];
        check_values ~extra:true "int x = 3; int y = x++ + x++;"
          [ ("x", int 5); ("y", int 7) ];
        check_values ~extra:true "int x = 1; int y = (x++ > 1 ? 10 : x);"
          [ ("x", int 2); ("y", int 2) ];
        check_array ~extra:true
          "int[] a = [1, 2, 3]; int i = 0; a[i++] := 9; a[i++] := a[i++];" "a"
          [ int 9; int 3; int 3 ]);
    test "abbreviations in loops, functions and procedures" (fun () ->
        check_values ~extra:true
          "int i = 0; int s = 0; while (i < 5) { s += i; i++; }"
          [ ("i", int 5); ("s", int 10) ];
        check_values ~extra:true
          "int f(int n) { n++; return n++; } int x = 4; int r = f(x++);"
          [ ("x", int 5); ("r", int 5) ];
        check_values ~extra:true
          "inc(int[] a) { a[0] := a[0] + 1; } int[] a = [0]; int i = 0; while \
           (i++ < 3) { inc(a); } int r = a[0];"
          [ ("i", int 4); ("r", int 3) ]);
    test "base programs run the same with the extended parser" (fun () ->
        List.iter
          (fun (src, bindings) ->
            check_values src bindings;
            check_values ~extra:true src bindings)
          [
            ( "int f(int n) (n < 2 ? n : f(n - 1) + f(n - 2)) int x = f(10);",
              [ ("x", int 55) ] );
            ("int x = 1 - -1; x := x - 1;", [ ("x", int 1) ]);
          ]);
    test "abbreviations are rejected by the base parser" (fun () ->
        List.iter (rejects parse) [ "x++;"; "int y = ++x;"; "x += 1;"; "d--;" ]);
    test "syntax errors" (fun () ->
        List.iter (rejects parse_extra)
          [
            "++x;";
            "int y = ++1;";
            "a[0]++;";
            "x += ;";
            "x++ ++;";
            "x, y += 1, 2;";
            "double d = 0.0; d++.;";
          ]);
    test "type errors" (fun () ->
        List.iter
          (check_fails ~extra:true TypeError)
          [
            "bool b = true; b++;";
            "char c = 'a'; int y = ++c;";
            "int x = 0; double d = x--;";
            "int[] a = [1]; a++;";
            "int f() 1 f++;";
            "z += 1;";
            "int y = z++;";
            "int x = 0; x += true;";
            "int x = 0; x += 1.0;";
          ]);
    test "errors point to the abbreviation" (fun () ->
        List.iter
          (fun (src, expected_msg, at) ->
            let msg, at' = error_at src in
            Alcotest.(check string) src expected_msg msg;
            Alcotest.(check (pair int int)) "position" at at')
          [
            ( "bool b = true;\nb++;",
              "Errore di tipo: '++' si applica solo a variabili int o double, \
               ma 'b' ha tipo bool",
              (2, 1) );
            ( "int x = 1;\ndouble y = x++;",
              "Errore di tipo: atteso il tipo double, trovato int",
              (2, 12) );
            ( "int[] a = [1];\nint y = a++;",
              "Errore di tipo: '++' si applica solo a variabili int o double, \
               ma 'a' ha tipo int[]",
              (2, 9) );
            ( "int f() 1 f--;",
              "Errore di tipo: 'f' è una funzione, non una variabile",
              (1, 11) );
            ("z += 1;", "Errore di tipo: 'z' non è stato dichiarato", (1, 1));
          ]);
    test "the expansion of x++ is traced at the abbreviation" (fun () ->
        let _, logs = run_traced ~extra:true "int x = 1;\nint y = x++;" in
        let cols =
          logs
          |> List.filter (fun Loc.{ at = { start_pos } } -> start_pos.line = 2)
          |> List.map (fun Loc.{ at = { start_pos } } -> start_pos.col)
        in
        Alcotest.(check (list int)) "columns" [ 9; 9; 1 ] cols);
    test "formatting" (fun () ->
        parses_as "x++; y--;" "x++;\ny--;";
        parses_as "int y = ++x + x++ - --d * d--;"
          "int y = (++x + x++) - (--d * d--);";
        parses_as "x += 1; x -= y++; x *= 2 + 3; x /= 4;"
          "x += 1;\nx -= y++;\nx *= 2 + 3;\nx /= 4;";
        parses_as "int y = -(--x); int z = -x--;"
          "int y = -(--x);\nint z = -x--;";
        parses_as "a[i++] := -x--;" "a[i++] := -x--;");
    test "formatting round-trips and keeps the meaning" (fun () ->
        List.iter
          (fun (src, bindings) ->
            let out = format src in
            Alcotest.(check string) src out (format out);
            check_values ~extra:true out bindings)
          [
            ( "int x = 5; double d = 1.5; int y = x++ + ++x; double e = d++ * \
               --d; x--; d++;",
              [
                ("x", int 6);
                ("y", int 12);
                ("d", double 2.5);
                ("e", double 2.25);
              ] );
            ( "int x = 3; int y = -(--x); int z = -x--; x += y; x *= 2; double \
               d = 1.0; double e = -(++d);",
              [
                ("x", int (-2));
                ("y", int (-2));
                ("z", int (-2));
                ("e", double (-2.0));
              ] );
            ( "int[] a = [1, 2, 3]; int i = 0; while (i < 3) { a[i] := a[i] + \
               1; i++; } int f(int n) { n += 1; return n--; } int r = f(i++);",
              [ ("i", int 4); ("r", int 4) ] );
          ]);
  ]
