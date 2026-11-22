open MaoLang
open Ast
open DataStructure
open TestCommon

let mem_of vs =
  List.fold_left (fun mem v -> Mem.alloc v mem |> fst) Mem.empty vs

let check_pp pp (v, s) = Alcotest.(check string) s s (Fmt.str "%a" pp v)

let tests =
  [
    test "alloc gives consecutive addresses" (fun () ->
        let mem, a0 = Mem.alloc "x" Mem.empty in
        let mem, a1 = Mem.alloc "y" mem in
        Alcotest.(check (list int64)) "addresses" [ 0L; 1L ] [ a0; a1 ];
        Alcotest.(check int64) "next_addr" 2L (Mem.next_addr mem);
        Alcotest.(check (option string)) "get" (Some "x") (Mem.get a0 mem));
    test "set overwrites and grows next_addr" (fun () ->
        let mem = mem_of [ "x" ] |> Mem.set 0L "z" |> Mem.set 5L "w" in
        Alcotest.(check (option string))
          "overwritten" (Some "z") (Mem.get 0L mem);
        Alcotest.(check int64) "next_addr" 6L (Mem.next_addr mem);
        Alcotest.(check (option string)) "hole" None (Mem.get 3L mem));
    test "memory is persistent" (fun () ->
        let mem = mem_of [ "x" ] in
        let _ = Mem.set 0L "z" mem in
        Alcotest.(check (option string)) "unchanged" (Some "x") (Mem.get 0L mem));
    test "to_seq_from" (fun () ->
        Alcotest.(check (list (pair int64 char)))
          "bindings"
          [ (1L, 'b'); (2L, 'c') ]
          (mem_of [ 'a'; 'b'; 'c' ] |> Mem.to_seq_from 1L |> List.of_seq));
    test "addresses print in hexadecimal" (fun () ->
        check_pp Int64Addr.pp (255L, "0xff"));
    test "default values" (fun () ->
        List.iter
          (fun (t, v) ->
            Alcotest.check rtval
              (Fmt.str "%a" Ast_pp.pp_base_type t)
              v
              (RTVal.default_from_type t))
          [
            (IntType, int 0);
            (BoolType, bool false);
            (DoubleType, double 0.0);
            (CharType, char '\000');
          ]);
    test "values print as Mao literals" (fun () ->
        List.iter (check_pp RTVal.pp)
          [
            (int (-3), "-3");
            (bool true, "true");
            (double 2.0, "2.0");
            (char 'a', "'a'");
            (char '\n', "'\\n'");
            (char '\000', "'\\0'");
            (char '\'', "'\\''");
            (RTVal.Addr 16L, "0x10");
          ]);
    test "types of values" (fun () ->
        List.iter (check_pp RTVal.pp_type)
          [ (int 1, "int"); (char 'a', "char"); (RTVal.Addr 0L, "array") ]);
  ]
