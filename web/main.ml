open Js_of_ocaml
open Fmt

let pp_table ?(sep = Fmt.cut) fmt logs =
  if logs = [] then Fmt.pf fmt ""
  else
    let headers = [ "event"; "descr" ] in
    let cells =
      List.map
        (fun Loc.{ it = MaoLang.Interpreter.{ event; descr }; at = loc } ->
          [ event; descr ])
        logs
    in
    let columns =
      List.init (List.length headers) (fun i ->
          List.map (fun row -> List.nth row i) cells)
    in
    let widths =
      List.map2
        (fun header column ->
          List.fold_left
            (fun width cell -> max width (String.length cell))
            (String.length header) column)
        headers columns
    in
    let print_row values =
      List.iter2
        (fun width value -> Fmt.pf fmt "| %-*s " width value)
        widths values;
      Fmt.pf fmt "|%a" sep ()
    in
    print_row headers;
    List.iter
      (fun width -> Fmt.pf fmt "|%s" (String.make (width + 2) '-'))
      widths;
    Fmt.pf fmt "|%a" sep ();
    List.iter print_row cells

let run_program (src : Js.js_string Js.t) =
  let lexbuf = Lexing.from_string (Js.to_string src) in
  let res, logs = MaoLang.run lexbuf in
  let logs_str = Fmt.to_to_string (pp_table ~sep:(Fmt.any "\n")) logs in
  let ok, output =
    match res with
    | Ok _ -> (true, logs_str)
    | Error msg -> (false, Fmt.str "%s\n%a" logs_str (Loc.pp Fmt.string) msg)
  in
  Js.Unsafe.obj
    [|
      ("ok", Js.Unsafe.inject (Js.bool ok));
      ("output", Js.Unsafe.inject (Js.string output));
    |]

let () =
  Js.Unsafe.set Js.Unsafe.global (Js.string "runProgram")
    (Js.wrap_callback run_program)
