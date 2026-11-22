open MaoLang
open DataStructure
open TestCommon

let events src =
  run_traced src |> snd
  |> List.map (fun Loc.{ it = { event } } ->
      Fmt.str "%a" DataStructure_pp.pp_event_name event)

let succeeds ~max_depth src = run_traced ~max_depth src |> fst |> Result.is_ok

let tests =
  [
    test "arithmetic" (fun () ->
        check_values "int a, b, c, d = 2 + 3 * 4, 7 / 2, 7 % 2, -7 / 2;"
          [ ("a", int 14); ("b", int 3); ("c", int 1); ("d", int (-3)) ]);
    test "64-bit integers" (fun () ->
        check_values
          "int x, y = 4611686018427387903 + 1, 9223372036854775807 + 1;"
          [
            ("x", RTVal.Int 4611686018427387904L); ("y", RTVal.Int Int64.min_int);
          ]);
    test "doubles and casts (8.1, 8.5)" (fun () ->
        check_values
          "double d = 1.5 * 2.0 - .5; int i, j = int(2.7), int(0.0 - 2.5);"
          [ ("d", double 2.5); ("i", int 2); ("j", int (-3)) ]);
    test "IEEE 754 special values (8.1)" (fun () ->
        check_values
          "double p, m, n = 1.0 / 0.0, -1.0 / 0.0, 0.0 / 0.0; bool a, b = n != \
           n, 5.0 % 0.0 != 5.0 % 0.0;"
          [
            ("p", double Float.infinity);
            ("m", double Float.neg_infinity);
            ("a", bool true);
            ("b", bool true);
          ];
        check_values "int i = int(-9223372036854775808.0);"
          [ ("i", RTVal.Int Int64.min_int) ]);
    test "comparisons and logic" (fun () ->
        check_values "bool a, b, c = 'a' < 'b', true xor true, !(1 == 2);"
          [ ("a", bool true); ("b", bool false); ("c", bool true) ]);
    test "left-to-right evaluation (E-Bop)" (fun () ->
        check_values
          "int[] a = [0]; int v = { a[0] := 1; return a[0]; } * 10 + a[0];"
          [ ("v", int 11) ]);
    test "short-circuit (a deliberate extension)" (fun () ->
        check_values "bool a, b = false && 1 / 0 == 0, true || 1 / 0 == 0;"
          [ ("a", bool false); ("b", bool true) ]);
    test "default values" (fun () ->
        check_values "int i; bool b; double d; char c;"
          [
            ("i", int 0);
            ("b", bool false);
            ("d", double 0.0);
            ("c", char '\000');
          ]);
    test "blocks and shadowing (4.4)" (fun () ->
        check_values
          "int x = 5; { int x = 2; x := 3; } int n = 0; while (n < 2) { int x \
           = x - 1; n := n + 1; }"
          [ ("x", int 5); ("n", int 2) ];
        check_values
          "int i, s; while (i < 3) { int t; t := t + 1; s := s + t; i := i + \
           1; }"
          [ ("s", int 3) ]);
    test "multiple declarations and assignments (C-Decls, C-Assigns)" (fun () ->
        check_values
          "int x = 1; int z; { int x, y = 10, x; z := y; } int w, bool w = 2, \
           true;"
          [ ("z", int 1); ("w", int 2) ];
        check_values "int x, y, z = 1, 2, 0; x, y := y, x; z, z := 7, 8;"
          [ ("x", int 2); ("y", int 1); ("z", int 7) ]);
    test "arrays (7.4, 7.5)" (fun () ->
        check_array "int[] a = new int[3]; a[1] := 7;" "a"
          [ int 0; int 7; int 0 ];
        check_array "char[] s = \"a\\n\";" "s" [ char 'a'; char '\n' ];
        check_values "int[] a = [-1, 2]; int n, s = a.length, a[0] + a[1];"
          [ ("n", int 2); ("s", int 1) ];
        check_values
          "int[] x = [42]; { int[] tmp = x; tmp[0] := 5; } int r = x[0];"
          [ ("r", int 5) ]);
    test "runtime errors" (fun () ->
        List.iter (check_fails RuntimeError)
          [
            "int x = 1 / 0;";
            "int x = 1 % 0;";
            "int i = int(1.0 / 0.0);";
            "int i = int(0.0 / 0.0);";
            "int i = int(10000000000000000000.0);";
            "int[] a = new int[0];";
            "int[] a = [1]; int x = a[1];";
            "int[] a = [1]; int x = a[0 - 1];";
            "int[] a = [1]; a[1] := 0;";
          ]);
    test "parameters (9.3)" (fun () ->
        check_values "p(int x) { x := 5; } int y = 1; p(y);" [ ("y", int 1) ];
        check_array "p(int[] a) { a[0] := 9; } int[] b = [1]; p(b);" "b"
          [ int 9 ];
        check_values
          "s(int[] a, int[] b) { a, b := b, a; } int[] x, y = [1], [2]; s(x, \
           y); int r = x[0];"
          [ ("r", int 1) ];
        check_values "int f(int x, int y) y int x = 7; int r = f(1, x);"
          [ ("r", int 7) ]);
    test "no parameters (a deliberate extension of 9.1, 10.1)" (fun () ->
        check_values
          "int g = 1; int f() 41 p() { int y = 1; } p(); int x = f() + g;"
          [ ("x", int 42) ];
        Alcotest.(check (list string))
          "events"
          [
            "dichiarazione di procedura";
            "inizio invocazione #1";
            "fine invocazione #1";
          ]
          (events "p() { skip; } p();"));
    test "recursion (11.3)" (fun () ->
        check_values
          "int f(int n) (n <= 1 ? 1 : n * f(n - 1)) int g(int n) { int h(int \
           n, int acc) (n <= 1 ? acc : h(n - 1, n * acc)) return h(n, 1); } \
           int a, b = f(10), g(10);"
          [ ("a", int 3628800); ("b", int 3628800) ]);
    test "logged events" (fun () ->
        Alcotest.(check (list string))
          "events"
          [
            "dichiarazione di funzione";
            "inizio chiamata #1";
            "dichiarazione";
            "fine chiamata #1";
            "dichiarazione";
            "if";
            "assegnamento";
          ]
          (events "int f(int n) n int x = f(1); if (x > 0) { x := 2; }"));
    test "steps and calls are numbered from 1" (fun () ->
        let src = "int f(int n) n int x = f(f(1));" in
        Alcotest.(check (list string))
          src
          [ "inizio chiamata #1"; "inizio chiamata #2" ]
          (events src |> List.filter (String.starts_with ~prefix:"inizio"));
        let table =
          Fmt.str "%a" DataStructure_pp.pp_logs (run_traced src |> snd)
        in
        let steps =
          String.split_on_char '\n' table
          |> List.filter_map (fun row -> Scanf.sscanf_opt row "| %d " Fun.id)
        in
        Alcotest.(check (list int)) "steps" (List.init 8 succ) steps);
    test "log limit" (fun () ->
        let res, logs =
          run_traced ~max_logs:5 "int n; while (true) { n := n + 1; }"
        in
        Alcotest.(check bool) "error" true (Result.is_error res);
        Alcotest.(check int) "logs" 5 (List.length logs));
    test "recursion limit" (fun () ->
        let proc = "p(int n) { if (n > 0) { p(n - 1); } } " in
        let func = "int f(int n) (n == 0 ? 0 : f(n - 1)) " in
        Alcotest.(check bool)
          "10 calls allowed" true
          (succeeds ~max_depth:10 (proc ^ "p(9);"));
        Alcotest.(check bool)
          "11 calls stopped" false
          (succeeds ~max_depth:10 (proc ^ "p(10);"));
        Alcotest.(check bool)
          "functions count too" false
          (succeeds ~max_depth:10 (func ^ "int x = f(10);"));
        Alcotest.(check bool)
          "sequential calls" true
          (succeeds ~max_depth:2 (proc ^ "p(1); p(1); p(1);")));
  ]
