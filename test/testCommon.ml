open MaoLang
open DataStructure

let test name f = Alcotest.test_case name `Quick f
let lexbuf = Lexing.from_string
let rtval = Alcotest.testable RTVal.pp ( = )
let int i = RTVal.Int (Int64.of_int i)
let bool b = RTVal.Bool b
let double d = RTVal.Double d
let char c = RTVal.Char c

type failure = ParseError | TypeError | RuntimeError | Crash of string

let pp_failure fmt = function
  | ParseError -> Fmt.string fmt "parse error"
  | TypeError -> Fmt.string fmt "type error"
  | RuntimeError -> Fmt.string fmt "runtime error"
  | Crash msg -> Fmt.pf fmt "crash (%s)" msg

let failure = Alcotest.testable pp_failure ( = )

let run_program src =
  let open Result.Syntax in
  let* ast =
    MaoLang.parse (lexbuf src) |> Result.map_error (Fun.const ParseError)
  in
  let* c_ast =
    TypeChecker.type_check_program ast |> Result.map_error (Fun.const TypeError)
  in
  match
    ReaderStateResultMonad.run ~env:Interpreter.no_limits
      (Interpreter.run_program c_ast)
  with
  | r, _ -> Result.map_error (Fun.const RuntimeError) r
  | exception Utils.Unreachable msg -> Error (Crash msg)

let state_of src =
  match run_program src with
  | Ok s -> s
  | Error f -> Alcotest.failf "%S: %a" src pp_failure f

let array_in s id =
  let arr_ptr = Interpreter.get_arr_ptr id s in
  let arr_len = Interpreter.get_arr_len id arr_ptr s |> Int64.to_int in
  List.init arr_len (fun idx ->
      Interpreter.lookup_addr
        (Interpreter.arr_elem_addr arr_ptr (Int64.of_int idx))
        s.mem)

let check_values src bindings =
  let s = state_of src in
  List.iter
    (fun (id, v) -> Alcotest.check rtval id v (Interpreter.lookup_var id s))
    bindings

let check_array src id vs =
  Alcotest.(check (list rtval)) id vs (array_in (state_of src) id)

let check_fails f src =
  Alcotest.(check (result unit failure))
    src (Error f)
    (run_program src |> Result.map ignore)

let run_traced ?max_logs ?max_depth src =
  match MaoLang.run ?max_logs ?max_depth (lexbuf src) with
  | Executed (r, logs) -> (r, logs)
  | NotExecuted Loc.{ it = msg } ->
      Alcotest.failf "%S: not executed (%s)" src msg
