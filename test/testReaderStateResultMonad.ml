open TestCommon
module RSR = ReaderStateResultMonad
open RSR.Syntax

let check_run_with ~env ok err msg expected m =
  Alcotest.(check (pair (result ok err) (list string)))
    msg expected (RSR.run ~env m)

let check_run ok err = check_run_with ~env:() ok err

let tests =
  [
    test "ok and error" (fun () ->
        check_run Alcotest.int Alcotest.string "ok" (Ok 1, []) (RSR.ok 1);
        check_run Alcotest.int Alcotest.string "error" (Error "e", [])
          (RSR.error "e"));
    test "ask reads the environment" (fun () ->
        let m =
          let* () = RSR.tell "a" in
          let* x = RSR.ask () in
          RSR.ok (x * 2)
        in
        check_run_with ~env:21 Alcotest.int Alcotest.string "ask"
          (Ok 42, [ "a" ]) m);
    test "tell keeps the order of the logs" (fun () ->
        let m =
          let* () = RSR.tell "a" in
          let* () = RSR.tell "b" in
          RSR.ok 3
        in
        check_run Alcotest.int Alcotest.string "logs" (Ok 3, [ "a"; "b" ]) m);
    test "logs before an error are kept" (fun () ->
        let m =
          let* () = RSR.tell "a" in
          let* () = RSR.error "stop" in
          RSR.tell "b"
        in
        check_run Alcotest.unit Alcotest.string "logs" (Error "stop", [ "a" ]) m);
    test "map and map_error" (fun () ->
        check_run Alcotest.int Alcotest.int "map" (Ok 2, []) (RSR.ok 1 =>> succ);
        check_run Alcotest.int Alcotest.int "map_error" (Error 2, [])
          (RSR.error 1 =&> succ);
        check_run Alcotest.int Alcotest.int "map_error on ok" (Ok 1, [])
          (RSR.ok 1 =&> succ));
    test "log ids count the logs" (fun () ->
        let m =
          let* () = RSR.tell "a" in
          let* () = RSR.tell "b" in
          RSR.get_log_id ()
        in
        check_run Alcotest.int Alcotest.string "log id" (Ok 2, [ "a"; "b" ]) m);
    test "call ids are independent of the logs" (fun () ->
        let m =
          let* c0 = RSR.incr_call_id () in
          let* () = RSR.tell "a" in
          let* c1 = RSR.incr_call_id () in
          RSR.ok (c0, c1)
        in
        check_run
          Alcotest.(pair int int)
          Alcotest.string "call ids"
          (Ok (0, 1), [ "a" ])
          m);
    test "call depth" (fun () ->
        let m =
          let* d1 = RSR.enter_call () in
          let* d2 = RSR.enter_call () in
          let* () = RSR.exit_call () in
          let* d2' = RSR.enter_call () in
          let* () = RSR.exit_call () in
          let* () = RSR.exit_call () in
          let* d1' = RSR.enter_call () in
          RSR.ok [ d1; d2; d2'; d1' ]
        in
        check_run
          Alcotest.(list int)
          Alcotest.string "depths"
          (Ok [ 1; 2; 2; 1 ], [])
          m);
  ]
