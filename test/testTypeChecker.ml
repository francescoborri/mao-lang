open MaoLang
open TestCommon

let type_checks src =
  match parse (lexbuf src) with
  | Ok ast -> Result.is_ok (TypeChecker.type_check_program ast)
  | Error _ -> Alcotest.failf "%S: parse error" src

let accepts =
  List.iter (fun src -> Alcotest.(check bool) src true (type_checks src))

let rejects =
  List.iter (fun src -> Alcotest.(check bool) src false (type_checks src))

let tests =
  [
    test "operators" (fun () ->
        accepts
          [
            "int x = 1 + 2 * 3 % 4;";
            "double d = 1.5 / 2.0;";
            "bool b = 1 < 2 && 'a' <= 'b' || true xor false;";
            "bool b = true == false;";
          ];
        rejects
          [
            "double d = 1 + 2.0;";
            "int x = true + 1;";
            "bool b = true < false;";
            "bool b = 1 && true;";
            "int x = !1;";
            "bool b = -true;";
          ]);
    test "casts (8.5)" (fun () ->
        accepts [ "double d = double(1);"; "int i = int(1.5) + int(2);" ];
        rejects [ "bool b = bool(1);"; "int i = int('a');" ]);
    test "declarations and assignments" (fun () ->
        accepts
          [
            "int i; bool b; double d; char c;";
            "int x, bool y = 1, true;";
            "int x, y; x, y := y, x;";
          ];
        rejects
          [
            "int[] a;";
            "int x = true;";
            "int x, y = 1;";
            "int x = y;";
            "x := 1;";
            "int x; x := true;";
            "int x; x := 1, 2;";
          ]);
    test "initializers see the starting environment (T-Decls)" (fun () ->
        rejects [ "int x, y = 1, x;" ];
        accepts
          [
            "int x, bool x = 1, true; int y = x + 1;";
            "bool a, int a; bool b = !a;";
          ]);
    test "scope (4.4, T-Return)" (fun () ->
        rejects
          [
            "{ int y = 1; } y := 2;";
            "if (true) { int z = 1; } else { z := 2; }";
            "while (true) { int z = 1; } z := 2;";
            "int x = { int y = 2; return y; }; y := 1;";
          ]);
    test "guards must be booleans" (fun () ->
        rejects
          [ "if (1) { skip; }"; "while (0) { skip; }"; "int m = (1 ? 2 : 3);" ]);
    test "arrays (7.3)" (fun () ->
        accepts
          [
            "int[] a = new int[3]; a[0] := a[1] + a.length;";
            "char[] s = \"ciao\"; char c = s[0];";
            "int[] a = [1]; int[] b = a;";
          ];
        rejects
          [
            "int[] a = [1, true];";
            "char[] s = \"\";";
            "int[] a = [1]; int x = a[true];";
            "int[] a = [1]; a[0] := 'c';";
            "int x; int n = x.length;";
            "int x; x[0] := 1;";
            "int[] a = [1]; bool b = a == a;";
            "int[] a = new int[true];";
          ]);
    test "procedures (9.2)" (fun () ->
        accepts
          [
            "p(int x, int[] a) { a[0] := x; } p(1, [2]);";
            "p(int n) { if (n > 0) { p(n - 1); } }";
            "p() { skip; } p();";
            "int g = 1; p(int x) { x := g; }";
          ];
        rejects
          [
            "p(int x) { y := x; } int y;";
            "p(int x) { skip; } p(1, 2);";
            "p(int x) { skip; } p(true);";
            "p(int a, int a) { skip; }";
            "p(int x) { skip; } int y = p(1);";
            "q(1);";
            "p() { skip; } p(1);";
            "p(int x) { skip; } p();";
          ]);
    test "functions (10.2, 11.2)" (fun () ->
        accepts
          [
            "int f(int n) (n <= 1 ? 1 : n * f(n - 1))";
            "int f(int n) { int g(int m) m + 1 return g(n); }";
            "int[] mk(int n) new int[n]";
            "int x = { int y = 2; return y + 1; };";
            "int f() 1 int x = f();";
            "int f() (true ? 1 : f())";
            "int g = 1; int f(int x) x + g";
          ];
        rejects
          [
            "int f(int x) x + g int g = 1;";
            "int f(int x) true";
            "int f(int a, bool a) 1";
            "int f(int x) x f(1);";
            "int f(int x) x int y = f(1, 2);";
            "int f(int a) a int g(int a) a f := g;";
            "int f(int a) a int y = f + 1;";
            "int m = (true ? 1 : false);";
            "int x = { int y = 2; return true; };";
            "int f() 1 int x = f(1);";
            "int f(int x) x int y = f();";
            "int f() 1 f();";
          ]);
  ]
