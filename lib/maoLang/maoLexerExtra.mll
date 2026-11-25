{
  open MaoParserExtra
  open Utils

  exception Error of string Loc.t

  let raise_lexer_error lexbuf ?(start_pos = Lexing.lexeme_start_p lexbuf)
      ?(end_pos = Lexing.lexeme_end_p lexbuf) fmt =
    let raise_error msg =
      raise (Error (Loc.with_positions ~start_pos ~end_pos msg))
    in
    Fmt.kstr raise_error fmt

  let non_ascii_error lexbuf c =
    let end_pos = Lexing.lexeme_end_p lexbuf in
    let start_pos =
      { end_pos with pos_cnum = end_pos.pos_cnum - String.length c }
    in
    raise_lexer_error lexbuf "carattere non ASCII '%s' non ammesso" ~start_pos
      ~end_pos c

  let parse_int lexbuf =
    match Int64.of_string_opt (Lexing.lexeme lexbuf) with
    | Some i -> i
    | None ->
        raise_lexer_error lexbuf "letterale intero '%s' troppo grande"
          (Lexing.lexeme lexbuf)

  let unescape_char = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | '0' -> '\000'
    | c -> c

  let unescape =
    let aux (acc, escaping) c =
      if escaping then (unescape_char c :: acc, false)
      else if c = '\\' then (acc, true)
      else (c :: acc, false)
    in
    String.to_seq
    >> Seq.fold_left aux ([], false)
    >> fst >> List.rev >> List.to_seq >> String.of_seq
}

let new_line = '\n' | "\r\n"

let white = [' ' '\t']+

let digit = ['0'-'9']

let int = digit+

let bool = "true" | "false"

let double = digit+ '.' digit+ | digit+ '.' | '.' digit+

let escape = '\\' ['n' 't' '0' '\\' '\'' '"']

let char = '\'' ([' '-'&' '('-'[' ']'-'~'] | escape) '\''

let string = ([' '-'!' '#'-'[' ']'-'~'] | '\t' | escape)*

let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let non_ascii = ['\192'-'\255'] ['\128'-'\191']* | ['\128'-'\191']

let line_comment = "//" [^'\n' '\r' '\128'-'\255']*

let open_multiline_comment = "/*"

let close_multiline_comment = "*/"

rule read = parse
| new_line { Lexing.new_line lexbuf; read lexbuf }
| white { read lexbuf }
| line_comment { read lexbuf }
| open_multiline_comment { comment lexbuf; read lexbuf }
| "++" { INT_INC }
| "++." { DOUBLE_INC }
| "--." { DOUBLE_DEC }
| "--" { INT_DEC }
| "+=" { PLUS_ASSIGN }
| "-=" { MINUS_ASSIGN }
| "*=" { TIMES_ASSIGN }
| "/=" { DIV_ASSIGN }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIV }
| "%" { MOD }
| "&&" { AND }
| "||" { OR }
| "xor" { XOR }
| "!" { NOT }
| "==" { EQ }
| "!=" { NEQ }
| "<=" { LEQ }
| "<" { LT }
| ">=" { GEQ }
| ">" { GT }
| "int" { INT_TYPE }
| "bool" { BOOL_TYPE }
| "double" { DOUBLE_TYPE }
| "char" { CHAR_TYPE }
| "skip" { SKIP }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "new" { NEW }
| "." white* "length" { DOT_LENGTH }  | "return" { RETURN }
| "=" { DECLARE }
| ":=" { ASSIGN }
| "," { COMMA }
| ";" { SEMICOLON }
| ":" { COLON }
| "?" { QUESTION_MARK }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| int { INT (parse_int lexbuf) }
| bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
| double { DOUBLE (float_of_string (Lexing.lexeme lexbuf)) }
| char { CHAR (String.get (unescape (Lexing.lexeme lexbuf)) 1) }
| '"' (string as s) '"' { STRING (unescape s) }
| ident { IDENT (Lexing.lexeme lexbuf) }
| eof { EOF }
| "." { raise_lexer_error lexbuf "dopo '.' è ammesso solo 'length'" }
| '\'' '\\' (_ as c) '\'' { raise_lexer_error lexbuf "sequenza di escape '\\%c' non valida" c }
| '\'' (non_ascii as c) { non_ascii_error lexbuf c }
| '\'' { raise_lexer_error lexbuf "carattere letterale non valido" }
| '"' string '\\' (_ as c) { raise_lexer_error lexbuf "sequenza di escape '\\%c' non valida" c }
| '"' string (non_ascii as c) { non_ascii_error lexbuf c }
| '"' string { raise_lexer_error lexbuf "stringa non terminata" }
| non_ascii as c { non_ascii_error lexbuf c }
| _ as c { raise_lexer_error lexbuf "carattere '%s' inatteso" (Char.escaped c) }

and comment = parse
| close_multiline_comment { () }
| new_line { Lexing.new_line lexbuf; comment lexbuf }
| non_ascii as c { non_ascii_error lexbuf c }
| eof { raise_lexer_error lexbuf "commento non terminato" }
| _ { comment lexbuf }


