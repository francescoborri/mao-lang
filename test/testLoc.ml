open TestCommon

let pos_of src n =
  let before = String.sub src 0 n in
  let line =
    1 + (String.to_seq before |> Seq.filter (( = ) '\n') |> Seq.length)
  in
  let bol = Option.fold ~none:0 ~some:succ (String.rindex_opt before '\n') in
  Lexing.{ pos_fname = ""; pos_lnum = line; pos_bol = bol; pos_cnum = n }

let src = "ab\ncde"

let msg =
  Loc.with_positions ~start_pos:(pos_of src 1) ~end_pos:(pos_of src 5) "errore"

let dummy = Loc.with_loc ~loc:Loc.dummy_loc "errore"
let pp_str pp = Fmt.str "%a" pp

let tests =
  [
    test "columns are 1-based" (fun () ->
        let Loc.{ start_pos; end_pos } = msg.at in
        Alcotest.(check (pair int int))
          "start" (1, 2)
          (start_pos.line, start_pos.col);
        Alcotest.(check (pair int int)) "end" (2, 3) (end_pos.line, end_pos.col);
        Alcotest.(check int) "end offset" 5 end_pos.offset);
    test "map_item keeps the location" (fun () ->
        let msg' = Loc.map_item String.length msg in
        Alcotest.(check int) "item" 6 (Loc.get_item msg');
        Alcotest.(check bool) "location" true (Loc.get_loc msg' = msg.at));
    test "pp" (fun () ->
        Alcotest.(check string)
          "start" "errore (riga 1, colonna 2)"
          (pp_str (Loc.pp Fmt.string) msg);
        Alcotest.(check string)
          "full" "errore (riga 1, colonna 2 - riga 2, colonna 3)"
          (pp_str (Loc.pp_full Fmt.string) msg));
    test "dummy_loc" (fun () ->
        Alcotest.(check bool) "is_dummy" true (Loc.is_dummy dummy.at);
        Alcotest.(check bool) "not is_dummy" false (Loc.is_dummy msg.at);
        Alcotest.(check string) "pp" "errore" (pp_str (Loc.pp Fmt.string) dummy);
        Alcotest.(check string)
          "pp_full" "errore"
          (pp_str (Loc.pp_full Fmt.string) dummy));
  ]
