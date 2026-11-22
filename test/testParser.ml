open MaoLang
open TestCommon

let format src =
  match parse (lexbuf src) with
  | Ok ast -> Fmt.str "%a" Ast_pp.pp_command ast
  | Error _ -> Alcotest.failf "%S: parse error" src

let parses_as src expected = Alcotest.(check string) src expected (format src)

let rejects src =
  Alcotest.(check bool) src true (Result.is_error (parse (lexbuf src)))

let round_trips src =
  let out = format src in
  Alcotest.(check string) src out (format out)

let tests =
  [
    test "precedence" (fun () ->
        parses_as "int x = 1 + 2 * 3 - 4;" "int x = (1 + (2 * 3)) - 4;";
        parses_as "bool b = a < b && c || !d xor e;"
          "bool b = ((a < b) && c) || ((!d) xor e);");
    test "unary minus binds tightest" (fun () ->
        parses_as "int x = -y * 3 - -2;" "int x = ((-y) * 3) - (-2);";
        parses_as "int[] a = [-1, 2, -3.5];" "int[] a = [-1, 2, -3.5];");
    test "declarations and assignments" (fun () ->
        List.iter
          (fun src -> parses_as src src)
          [
            "int x;";
            "int x, y = 1, 2;";
            "int x, bool y = 1, true;";
            "x, y := y, x;";
          ]);
    test "commands" (fun () ->
        parses_as "if (b) { skip; }" "if (b) {\n    skip;\n}";
        parses_as "int[] a = new int[3]; a[0] := a.length;"
          "int[] a = new int[3];\na[0] := a.length;";
        parses_as {|char[] s = "a\n";|} {|char[] s = "a\n";|};
        parses_as "int i = int(2.5);" "int i = int(2.5);");
    test "functions and procedures" (fun () ->
        parses_as "int f(int a, b) (a > b ? a : b)"
          "int f(int a, int b) (a > b ? a : b)";
        parses_as "p(int[] a) { a[0] := f(1, 2); } p(x);"
          "p(int[] a) {\n    a[0] := f(1, 2);\n}\np(x);";
        parses_as "int f(int a) { return a; }" "int f(int a) a";
        parses_as "int f() 1 p() { skip; } p(); int x = f();"
          "int f() 1\np() {\n    skip;\n}\np();\nint x = f();");
    test "syntax errors" (fun () ->
        List.iter rejects
          [
            "int x = ;";
            "x = 1;";
            "if (b) { skip; } else if (c) { skip; }";
            "int[] a = [];";
            "int x = 1";
            "";
          ]);
    test "formatting round-trips" (fun () ->
        List.iter round_trips
          [
            {|char a, b, c, d = '\n', '\'', '\\', '\0'; char[] s = "x\"y\tz";|};
            "double x, y = 100000000000000000000.0, 0.00001;";
            "int[] a = [-1, 2]; int x = -(3) * -a[0];";
            "int f(int n) { int r = 1; while (n > 1) { r, n := r * n, n - 1; } \
             return r; }";
          ]);
  ]
