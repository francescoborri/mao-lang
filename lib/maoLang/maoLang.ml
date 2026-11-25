module Ast = MaoAst
module Ast_pp = MaoAst_pp
module DataStructure = MaoDataStructure
module DataStructure_pp = MaoDataStructure_pp
module Lexer = MaoLexer
module Parser = MaoParser
module ExtraLexer = MaoLexerExtra
module ExtraParser = MaoParserExtra
module TypeChecker = MaoTypeChecker
module Interpreter = MaoInterpreter
module Trace = MaoTrace
open Utils

type execution_result =
  | Executed of
      (DataStructure.state, string Loc.t) result * DataStructure.log Loc.t list
  | NotExecuted of string Loc.t

module I = MaoParser.MenhirInterpreter
module IExtra = MaoParserExtra.MenhirInterpreter

module type PARSER = sig
  type token

  module I :
    MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token

  val program : Lexing.position -> Ast.command I.checkpoint
  val state_hint : Ast.command I.checkpoint -> string option
end

(* hints for tokens that are wrong wherever they are rejected, so they take
   precedence over the hints of maoParser.messages, which depend on the state *)
let token_hint = function
  | "=" ->
      Some
        "Il simbolo '=' si usa solo per inizializzare una variabile nella sua \
         dichiarazione: per assegnare un valore si usa ':=', per confrontare \
         '=='"
  | "?" ->
      Some
        "L'espressione condizionale va scritta tra parentesi: (condizione ? e1 \
         : e2)"
  | "return" ->
      Some
        "'return' si usa solo alla fine di un blocco che calcola un valore, { \
         ... return e; }, ad esempio come corpo di una funzione: le procedure \
         non restituiscono valori"
  | _ -> None

module MaoParserModule = struct
  type token = MaoParser.token

  module I = MaoParser.MenhirInterpreter

  let program = MaoParser.Incremental.program

  let state_hint = function
    | I.HandlingError env -> (
        match MaoParser_messages.message (I.current_state_number env) with
        | msg -> Some (String.trim msg)
        | exception Not_found -> None)
    | _ -> None
end

module MaoParserExtraModule = struct
  type token = MaoParserExtra.token

  module I = MaoParserExtra.MenhirInterpreter

  let program = MaoParserExtra.Incremental.program

  let state_hint = function
    | I.HandlingError env -> (
        match MaoParserExtra_messages.message (I.current_state_number env) with
        | msg -> Some (String.trim msg)
        | exception Not_found -> None)
    | _ -> None
end
(* the rejected token is the last one read by the lexer *)

let syntax_error state_hint lexbuf checkpoint =
  let token = Lexing.lexeme lexbuf in
  let unexpected =
    match token with
    | "" -> "fine del programma inattesa"
    | _ when token.[0] = '\'' || token.[0] = '"' -> Fmt.str "%s inatteso" token
    | _ -> Fmt.str "'%s' inatteso" token
  in
  let hint =
    match token_hint token with None -> state_hint checkpoint | hint -> hint
  in
  Fmt.str "Errore di sintassi: %s%a" unexpected
    Fmt.(option (any ". " ++ string))
    hint
  |> Loc.with_positions
       ~start_pos:(Lexing.lexeme_start_p lexbuf)
       ~end_pos:(Lexing.lexeme_end_p lexbuf)
  |> Result.error

let parse lexbuf =
  let supplier = I.lexer_lexbuf_to_supplier MaoLexer.read lexbuf in
  let start = MaoParser.Incremental.program lexbuf.Lexing.lex_curr_p in
  try
    I.loop_handle Result.ok
      (syntax_error MaoParserModule.state_hint lexbuf)
      supplier start
  with MaoLexer.Error msg ->
    msg |> Loc.map_item (Fmt.str "Errore lessicale: %s") |> Result.error

let type_check =
  MaoTypeChecker.type_check_program
  >> Result.map_error (Loc.map_item (Fmt.str "Errore di tipo: %s"))

let parse_type_check_with (type token)
    (module P : PARSER with type token = token) lexer lexbuf =
  let supplier = P.I.lexer_lexbuf_to_supplier lexer lexbuf in
  let start = P.program lexbuf.Lexing.lex_curr_p in
  try
    let parsed =
      P.I.loop_handle Result.ok
        (syntax_error P.state_hint lexbuf)
        supplier start
    in
    Result.bind parsed type_check
  with
  | MaoLexer.Error msg ->
      msg |> Loc.map_item (Fmt.str "Errore lessicale: %s") |> Result.error
  | MaoLexerExtra.Error msg ->
      msg |> Loc.map_item (Fmt.str "Errore lessicale: %s") |> Result.error

let parse_type_check lexbuf =
  parse_type_check_with (module MaoParserModule) MaoLexer.read lexbuf

let parse_type_check_extra lexbuf =
  parse_type_check_with (module MaoParserExtraModule) MaoLexerExtra.read lexbuf

let execute c_ast =
  let open ReaderStateResultMonad.Syntax in
  MaoInterpreter.run_program c_ast
  =&> Loc.map_item (Fmt.str "Errore di esecuzione: %s")

let run ?(max_logs = max_int) ?(max_depth = max_int) lexbuf =
  let executed (r, logs) = Executed (r, logs) in
  let not_executed e = NotExecuted e in
  let module RSR = ReaderStateResultMonad in
  let env = MaoInterpreter.{ max_logs; max_depth } in
  Result.retract
  @@
    try
      parse_type_check lexbuf
      |> Result.map (execute >> RSR.run ~env >> executed)
      |> Result.map_error not_executed
    with Utils.Unreachable msg ->
      msg
      |> Fmt.str "Errore interno dell'interprete: %s"
      |> Loc.with_loc ~loc:Loc.dummy_loc
      |> RSR.error |> RSR.run ~env |> executed |> Result.error

let run_extra ?(max_logs = max_int) ?(max_depth = max_int) lexbuf =
  let executed (r, logs) = Executed (r, logs) in
  let not_executed e = NotExecuted e in
  let module RSR = ReaderStateResultMonad in
  let env = MaoInterpreter.{ max_logs; max_depth } in
  Result.retract
  @@
    try
      parse_type_check_extra lexbuf
      |> Result.map (execute >> RSR.run ~env >> executed)
      |> Result.map_error not_executed
    with Utils.Unreachable msg ->
      msg
      |> Fmt.str "Errore interno dell'interprete: %s"
      |> Loc.with_loc ~loc:Loc.dummy_loc
      |> RSR.error |> RSR.run ~env |> executed |> Result.error
