open MaoLang
open Utils
open TestCommon

let program =
  {|int f(int n) (n <= 1 ? 1 : n * f(n - 1))
fill(int[] a, int v) {
    int i = 0;
    while (i < a.length) { a[i] := v; i := i + 1; }
}
int[] a = new int[3];
fill(a, 7);
int x = f(3);|}

let trace = run_traced program |> snd |> Trace.of_logs
let n = Trace.length trace
let last = n - 1
let items Trace.Page.{ items } = items
let entries t = Trace.entries t ~offset:0 ~limit:(Trace.view_length t) |> items

let find_entry id =
  entries trace |> List.find (fun Trace.Entry.{ name } -> name = id)

let find_var id =
  Trace.vars trace last ~query:"" ~offset:0 ~limit:100
  |> items
  |> List.find (fun Trace.Var.{ name } -> name = id)

let owners = items >> List.map (fun Trace.Cell.{ owner } -> owner)
let stores = Trace.filter trace ~kinds:[ Trace.Kind.Store ] ~query:""

let tests =
  [
    test "entries" (fun () ->
        let Trace.Entry.{ descr } = find_entry "inizio chiamata #2" in
        Alcotest.(check int) "length" n (List.length (entries trace));
        Alcotest.(check string) "call description" "f(...)" descr;
        Alcotest.(check bool)
          "end of invocation" true
          (List.exists
             (fun Trace.Entry.{ name } -> name = "fine invocazione #1")
             (entries trace)));
    test "depth" (fun () ->
        let depth_of id =
          let Trace.Entry.{ depth } = find_entry id in
          depth
        in
        Alcotest.(check int) "max depth" 3 (Trace.max_depth trace);
        Alcotest.(check int)
          "begin and end of a call"
          (depth_of "inizio chiamata #3")
          (depth_of "fine chiamata #3");
        Alcotest.(check int) "top level" 0 (depth_of "inizio invocazione #1"));
    test "pages" (fun () ->
        let Trace.Page.{ items; offset; total } =
          Trace.entries trace ~offset:2 ~limit:3
        in
        let Trace.Page.{ offset = last_offset } =
          Trace.entries trace ~offset:(n + 10) ~limit:5
        in
        Alcotest.(check (list int))
          "indices" [ 2; 3; 4 ]
          (List.map (fun Trace.Entry.{ index } -> index) items);
        Alcotest.(check (pair int int)) "offset and total" (2, n) (offset, total);
        Alcotest.(check int)
          "past the end is the last page"
          (last / 5 * 5)
          last_offset);
    test "kind counts" (fun () ->
        let counts = Trace.kind_counts trace in
        Alcotest.(check (option int))
          "calls" (Some 6)
          (List.assoc_opt Trace.Kind.Call counts);
        Alcotest.(check (option int))
          "invocations" (Some 2)
          (List.assoc_opt Trace.Kind.Invoke counts);
        Alcotest.(check int)
          "total" n
          (List.fold_left ( + ) 0 (List.map snd counts)));
    test "filter by kind and text" (fun () ->
        let by_text =
          Trace.filter trace ~kinds:Trace.Kind.all ~query:"  A[1] "
        in
        let reset = Trace.filter stores ~kinds:Trace.Kind.all ~query:"" in
        Alcotest.(check int) "stores" 3 (Trace.view_length stores);
        Alcotest.(check int) "text" 1 (Trace.view_length by_text);
        Alcotest.(check int) "reset" n (Trace.view_length reset));
    test "position in a filtered view" (fun () ->
        let Trace.Entry.{ index = first } = List.hd (entries stores) in
        Alcotest.(check int) "before" 0 (Trace.position stores 0);
        Alcotest.(check int) "first" 0 (Trace.position stores first);
        Alcotest.(check int) "after" 2 (Trace.position stores last));
    test "variables" (fun () ->
        let Trace.Var.{ value; changed } = find_var "x" in
        let Trace.Var.{ ty = a_ty; array; changed = a_changed } =
          find_var "a"
        in
        let Trace.Var.{ ty = f_ty; addr } = find_var "f" in
        Alcotest.(check string) "x" "6" value;
        Alcotest.(check bool) "x changed" true changed;
        Alcotest.(check string) "a type" "int[]" a_ty;
        Alcotest.(check (option string))
          "a preview" (Some "lunghezza 3: [7, 7, 7]") (Option.map snd array);
        Alcotest.(check bool) "a not changed" false a_changed;
        Alcotest.(check string) "f type" "funzione" f_ty;
        Alcotest.(check bool) "f has no address" true (Option.is_none addr);
        Alcotest.(check int)
          "query" 1
          (Trace.vars trace last ~query:"FILL" ~offset:0 ~limit:10).total);
    test "memory" (fun () ->
        let all = Trace.memory trace last ~query:"" ~offset:0 ~limit:100 in
        let sevens = Trace.memory trace last ~query:"7" ~offset:0 ~limit:10 in
        Alcotest.(check int) "one page" all.total (List.length (items all));
        List.iter
          (fun owner ->
            Alcotest.(check bool) owner true (List.mem owner (owners all)))
          [ "a"; "a.length"; "a[0]"; "a[2]" ];
        Alcotest.(check (list string))
          "array cell type" [ "int[]" ]
          (items all
          |> List.filter_map (fun Trace.Cell.{ owner; ty } ->
              if owner = "a" then Some ty else None));
        Alcotest.(check (list string))
          "query"
          [ ""; "a[0]"; "a[1]"; "a[2]" ]
          (List.sort compare (owners sevens)));
  ]
