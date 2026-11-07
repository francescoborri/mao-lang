{
  open MaoParser
  exception Error of string Loc.t

  let error lexbuf c =
    raise
      (Error
         (Loc.with_positions
            ~start_pos:(Lexing.lexeme_start_p lexbuf)
            ~end_pos:(Lexing.lexeme_end_p lexbuf)
            (Fmt.str "unexpected character '%c'" c)))
}

let return = '\n' | "\r\n"
let white = [' ' '\t']+

let digit = ['0'-'9']
let int = digit+
let bool = "true" | "false"
let double = digit+ '.' digit+ | digit+ '.' | '.' digit+
let char = '\'' ['a'-'z' 'A'-'Z' '0'-'9'] '\''
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  (* Blank characters *)
  | return { Lexing.new_line lexbuf; read lexbuf }
  | white { read lexbuf }
  (* Operators and symbols *)
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
  (* Keywords *)
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  | "double" { DOUBLE_TYPE }
  | "char" { CHAR_TYPE }
  | "skip" { SKIP }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "new" { NEW }
  | "length" { LENGTH }
  | "return" { RETURN }
  (* Symbols and delimiters *)
  | "=" { DECLARE }
  | ":=" { ASSIGN }
  | "." { DOT }
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
  (* Literals and identifiers *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | double { DOUBLE (float_of_string (Lexing.lexeme lexbuf)) }
  | char { CHAR (String.get (Lexing.lexeme lexbuf) 1) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ as c { error lexbuf c }


