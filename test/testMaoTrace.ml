open MaoLang

let fib =
  {|int fib_rec(int n) {
    int r;
    if (n <= 1) { r := n; } else { r := fib_rec(n - 1) + fib_rec(n - 2); }
    return r;
}
fib_iter(int n, int[] ptr) {
    int f0, f1 = 0, 1;
    while (n > 0) { n, f0, f1 := n - 1, f1, f0 + f1; }
    ptr[0] := f0;
}
int n, int[] ptr = 10, new int[1];
fib_iter(n, ptr);
int x, y = fib_rec(n), ptr[0];
bool ok = x == y;|}

let run ?max_logs src =
  let res, logs = run ?max_logs (Lexing.from_string src) in
  (res, Trace.of_logs logs)

let check name b =
  if not b then (
    Fmt.epr "FAIL: %s@." name;
    exit 1)

let error_contains ~sub = function
  | Error Loc.{ it = msg } -> Trace.contains ~sub msg
  | Ok _ -> false

let test_locations () =
  let src =
    "int x = 1; // nota\n/* blocco\n   su due righe */\nint y = x / 0;"
  in
  let res, t = run src in
  check "comments: one log before the error" (Trace.length t = 1);
  (match res with
  | Error Loc.{ at = { start_pos } } ->
      check "comments: error position" (start_pos.line = 4 && start_pos.col = 13)
  | Ok _ -> check "comments: division by zero must fail" false);
  let res, _ = run "int x = 1; /* mai chiuso" in
  check "unterminated comment" (error_contains ~sub:"unterminated" res)

let test_entries () =
  let res, t = run fib in
  let n = Trace.length t in
  let entries = (Trace.entries t ~offset:0 ~limit:n).items in
  check "fib: run ok" (Result.is_ok res);
  check "fib: has logs" (n > 100 && List.length entries = n);
  check "fib: depth inside calls" (Trace.max_depth t > 0);
  let names = List.map (fun Trace.Entry.{ name } -> name) entries in
  check "fib: names carry the call id"
    (List.mem "inizio invocazione #0" names
    && List.mem "fine invocazione #0" names);
  let depth_of name =
    List.find (fun Trace.Entry.{ name = name' } -> name' = name) entries
    |> fun Trace.Entry.{ depth } -> depth
  in
  check "fib: begin and end at the same depth"
    (depth_of "inizio chiamata #1" = depth_of "fine chiamata #1")

let test_pages () =
  let _, t = run fib in
  let n = Trace.length t in
  let Trace.{ items; offset; total } = Trace.entries t ~offset:10 ~limit:5 in
  check "page: window" (List.length items = 5 && offset = 10 && total = n);
  check "page: indices"
    (List.map (fun Trace.Entry.{ index } -> index) items
    = [ 10; 11; 12; 13; 14 ]);
  let Trace.{ items; offset } = Trace.entries t ~offset:(n + 100) ~limit:10 in
  check "page: past the end gives the last page"
    (offset = (n - 1) / 10 * 10 && List.length items = n - offset)

let test_filter () =
  let _, t = run fib in
  let n = Trace.length t in
  let calls = Trace.filter t ~kinds:[ Trace.Kind.Call ] ~query:"" in
  let c = Trace.view_length calls in
  check "filter: kinds" (c > 0 && c < n);
  let Trace.{ items } = Trace.entries calls ~offset:0 ~limit:c in
  check "filter: only calls"
    (List.for_all (fun Trace.Entry.{ kind } -> kind = Trace.Kind.Call) items);
  let first = (List.hd items).index in
  check "position: hit" (Trace.position calls first = 0);
  check "position: before the first entry" (Trace.position calls 0 = 0);
  check "position: between entries" (Trace.position calls (first + 1) = 0);
  let by_text = Trace.filter t ~kinds:Trace.Kind.all ~query:"  FIB_REC( " in
  check "filter: query" (Trace.view_length by_text > 0);
  let reset = Trace.filter calls ~kinds:Trace.Kind.all ~query:"" in
  check "filter: reset"
    (Trace.view_length reset = n && Trace.position reset 7 = 7)

let test_vars () =
  let _, t = run fib in
  let last = Trace.length t - 1 in
  let Trace.{ items = vars; total } =
    Trace.vars t last ~query:"" ~offset:0 ~limit:100
  in
  let find id = List.find (fun Trace.Var.{ name } -> name = id) vars in
  check "vars: count" (total = List.length vars);
  check "vars: values" ((find "x").value = "55" && (find "y").value = "55");
  check "vars: last change" ((find "ok").changed && not (find "x").changed);
  check "vars: array"
    ((find "ptr").ty = "int[]" && Option.is_some (find "ptr").array);
  check "vars: function"
    ((find "fib_rec").ty = "funzione" && (find "fib_rec").addr = None);
  let Trace.{ items } = Trace.vars t last ~query:"fib" ~offset:0 ~limit:100 in
  check "vars: query" (List.length items = 2);
  let Trace.{ items } = Trace.vars t 0 ~query:"" ~offset:0 ~limit:100 in
  check "vars: everything changed at the first log"
    (List.for_all (fun Trace.Var.{ changed } -> changed) items)

let test_memory () =
  let _, t = run fib in
  let last = Trace.length t - 1 in
  let Trace.{ items; total } =
    Trace.memory t last ~query:"" ~offset:0 ~limit:4
  in
  check "memory: page" (List.length items = 4 && total > 4);
  let Trace.{ items = cells } =
    Trace.memory t last ~query:"" ~offset:0 ~limit:total
  in
  let owned id = List.exists (fun Trace.Cell.{ owner } -> owner = id) cells in
  check "memory: owners" (owned "ptr" && owned "ptr.length" && owned "ptr[0]");
  check "memory: few changes"
    (List.length (List.filter (fun Trace.Cell.{ changed } -> changed) cells) = 1);
  let Trace.{ items } = Trace.memory t last ~query:"ptr" ~offset:0 ~limit:10 in
  check "memory: query by owner" (List.length items = 3);
  let Trace.{ items } = Trace.memory t last ~query:"55" ~offset:0 ~limit:10 in
  check "memory: query by value"
    (items <> []
    && List.for_all (fun Trace.Cell.{ value } -> value = "55") items)

let test_limits () =
  let res, t = run ~max_logs:50 fib in
  check "limit: partial trace" (Trace.length t = 50);
  check "limit: error" (error_contains ~sub:"log limit" res);
  let res, t = run ~max_logs:1000 "int n = 0; while (true) { n := n + 1; }" in
  check "limit: divergent program" (Result.is_error res && Trace.length t = 1000);
  let res, t = run "int x = ;" in
  check "parse error: empty trace" (Result.is_error res && Trace.length t = 0);
  check "parse ignores types"
    (Result.is_ok (MaoLang.parse (Lexing.from_string "int x = true;")));
  check "type_check checks types"
    (Result.is_error (MaoLang.type_check (Lexing.from_string "int x = true;")))

let () =
  test_locations ();
  test_entries ();
  test_pages ();
  test_filter ();
  test_vars ();
  test_memory ();
  test_limits ();
  print_endline "all trace tests passed"
