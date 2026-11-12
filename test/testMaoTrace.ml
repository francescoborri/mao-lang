open MaoLang

let run ?max_logs src =
  let res, logs = run ?max_logs (Lexing.from_string src) in
  (res, Trace.of_logs logs)

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

let check name b = if not b then (Printf.eprintf "FAIL: %s\n" name; exit 1)

let () =
  (* ---- a complete run *)
  let res, t = run fib in
  check "run ok" (Result.is_ok res);
  let n = Trace.length t in
  check "has logs" (n > 100);
  (* every begin has a matching end, with the same call id *)
  for i = 0 to n - 1 do
    let r = Trace.row t i in
    if r.call_end >= 0 then begin
      let e = Trace.row t r.call_end in
      check "end after begin" (e.index > r.index);
      check "same call id" (e.call = r.index && r.call = r.index);
      check "same depth" (e.depth = r.depth)
    end
  done;
  check "depth > 0 inside calls" (Trace.max_depth t > 0);
  (* out_end of an inner entry is the end of the enclosing call *)
  let inner = ref (-1) in
  for i = n - 1 downto 0 do if (Trace.row t i).out_end >= 0 then inner := i done;
  check "has nested entry" (!inner >= 0);
  (* ---- final environment & memory *)
  let last = n - 1 in
  let env = Trace.env_rows t last in
  let find v = List.find (fun (r : Trace.env_row) -> r.var = v) env in
  check "x = 55" ((find "x").value = "55" && (find "y").value = "55");
  check "ok = true" ((find "ok").value = "true");
  check "ptr is array" ((find "ptr").vkind = "array" && (find "ptr").ty = "int[]");
  check "fib_rec is func" ((find "fib_rec").vkind = "func");
  let mp = Trace.mem_page t ~log:last ~start:0 ~count:4 in
  check "page size" (List.length mp.cells = 4 && mp.bound > 4);
  let mp2 = Trace.mem_page t ~log:last ~start:(mp.bound - 3) ~count:100 in
  check "last page short" (List.length mp2.cells = 3);
  (* labels: ptr cell is labelled, array elements too *)
  let all = Trace.mem_page t ~log:last ~start:0 ~count:mp.bound in
  check "var label" (List.exists (fun (c : Trace.cell) -> c.label = "ptr") all.cells);
  check "elem label" (List.exists (fun (c : Trace.cell) -> c.label = "ptr[0]" && c.cvalue = "55") all.cells);
  (* typed search *)
  (match Trace.mem_search t ~log:last ~query:"55" ~limit:2 with
  | Some (cells, _) -> check "search 55" (List.length cells >= 1)
  | None -> check "query parses" false);
  check "bad query" (Trace.mem_search t ~log:last ~query:"abc" ~limit:5 = None);
  (match Trace.mem_search t ~log:last ~query:"0" ~limit:3 with
  | Some (cells, trunc) -> check "limit" (List.length cells = 3 && trunc)
  | None -> check "query parses" false);
  (* changed flag: last log differs from previous at exactly the written cell *)
  let a = Trace.mem_page t ~log:last ~start:0 ~count:mp.bound in
  let changed = List.filter (fun (c : Trace.cell) -> c.changed) a.cells in
  check "few changes" (List.length changed >= 1 && List.length changed <= 2);
  (* ---- filter *)
  let c = Trace.set_filter t ~kinds:[ "call" ] ~text:"" in
  check "filter kinds" (c > 0 && c < n);
  let p = Trace.page t ~offset:0 ~limit:5 in
  check "page rows are calls" (List.for_all (fun (r : Trace.row) -> r.kind = "call") p);
  let first = (List.hd p).index in
  check "locate hit" (Trace.locate t first = 0);
  check "locate miss" (Trace.locate t 0 = -1 && Trace.locate t (first + 1) = -2);
  let c2 = Trace.set_filter t ~kinds:Trace.all_kinds ~text:"FIB_REC(" in
  check "text filter" (c2 > 0);
  let c3 = Trace.set_filter t ~kinds:Trace.all_kinds ~text:"" in
  check "reset" (c3 = n && Trace.view_length t = n && Trace.locate t 7 = 7);
  (* ---- log cap keeps partial trace and reports an error *)
  let res, t = run ~max_logs:50 fib in
  check "capped" (Trace.length t = 50);
  (match res with
  | Error { Loc.it; _ } -> check "msg" (Trace.contains ~sub:"log limit" it)
  | Ok _ -> check "should be error" false);
  (* divergent program is stopped *)
  let res, t = run ~max_logs:1000 "int n = 0; while (true) { n := n + 1; }" in
  check "divergent stopped" (Result.is_error res && Trace.length t = 1000);
  (* syntax / type errors give an empty trace *)
  let res, t = run "int x = ;" in
  check "parse error" (Result.is_error res && Trace.length t = 0);
  (* formatter entry point: parse only *)
  (match MaoLang.parse (Lexing.from_string "int x = true;") with
  | Ok _ -> () | Error _ -> check "parse ignores types" false);
  (match MaoLang.type_check (Lexing.from_string "int x = true;") with
  | Error _ -> () | Ok _ -> check "type_check still checks" false);
  print_endline "all trace tests passed"
