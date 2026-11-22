open Utils
open TestCommon

let fold_add acc x y = if x + y < 0 then Error (x, y) else Ok (acc + x + y)
let fold_add_right x y acc = fold_add acc x y
let sum_result = Alcotest.(result int (pair int int))

let check_float_string (f, s) =
  Alcotest.(check string) (Fmt.str "%g" f) s (shortest_float_string f)

let tests =
  [
    test "composition" (fun () ->
        Alcotest.(check int) ">>" 6 (((fun x -> x + 1) >> fun x -> x * 2) 2);
        Alcotest.(check (pair int string))
          "***" (2, "a")
          ((succ *** String.lowercase_ascii) (1, "A")));
    test "unreachable raises" (fun () ->
        Alcotest.check_raises "message" (Unreachable "x = 1") (fun () ->
            unreachable "x = %d" 1));
    test "result_fold_rightM" (fun () ->
        let cons x acc = Ok (x :: acc) in
        let stop_at_2 x acc = if x = 2 then Error x else Ok (x :: acc) in
        Alcotest.(check (result (list int) int))
          "right to left"
          (Ok [ 1; 2; 3 ])
          (result_fold_rightM cons [ 1; 2; 3 ] []);
        Alcotest.(check (result (list int) int))
          "stops at the first error" (Error 2)
          (result_fold_rightM stop_at_2 [ 1; 2; 3 ] []));
    test "result_fold_left2M" (fun () ->
        Alcotest.check sum_result "ok" (Ok 10)
          (result_fold_left2M fold_add 0 [ 1; 2 ] [ 3; 4 ]);
        Alcotest.check sum_result "first error"
          (Error (1, -5))
          (result_fold_left2M fold_add 0 [ 1; 2 ] [ -5; -6 ]));
    test "result_fold_right2M" (fun () ->
        Alcotest.check sum_result "ok" (Ok 10)
          (result_fold_right2M fold_add_right [ 1; 2 ] [ 3; 4 ] 0);
        Alcotest.check sum_result "first error"
          (Error (2, -6))
          (result_fold_right2M fold_add_right [ 1; 2 ] [ -5; -6 ] 0));
    test "folds reject lists of different lengths" (fun () ->
        match result_fold_left2M fold_add 0 [ 1 ] [] with
        | _ -> Alcotest.fail "no exception"
        | exception Unreachable _ -> ());
    test "shortest_float_string: shortest digits" (fun () ->
        List.iter check_float_string
          [
            (0.0, "0.0");
            (-0.0, "-0.0");
            (1.0, "1.0");
            (-2.5, "-2.5");
            (0.1, "0.1");
            (1. /. 3., "0.3333333333333333");
            (123456789.125, "123456789.125");
          ]);
    test "shortest_float_string: no exponent" (fun () ->
        List.iter check_float_string
          [
            (100.0, "100.0");
            (1e20, "100000000000000000000.0");
            (1e-5, "0.00001");
            (1.2345e-10, "0.00000000012345");
          ]);
    test "shortest_float_string: special values (8.1)" (fun () ->
        List.iter check_float_string
          [
            (Float.infinity, "+\u{221E}");
            (Float.neg_infinity, "-\u{221E}");
            (Float.nan, "NaN");
            (-.Float.nan, "NaN");
          ]);
    test "utf8_length counts code points" (fun () ->
        List.iter
          (fun (s, n) -> Alcotest.(check int) s n (utf8_length s))
          [ ("", 0); ("abc", 3); ("+\u{221E}", 2); ("perch\u{E9}", 6) ]);
    test "shortest_float_string: reads back exactly" (fun () ->
        List.iter
          (fun f ->
            let s = shortest_float_string f in
            Alcotest.(check (float 0.)) s f (float_of_string s);
            Alcotest.(check bool) s false (String.contains s 'e'))
          [ max_float; min_float; 5e-324; 9007199254740993.0; 1e23 ]);
  ]
