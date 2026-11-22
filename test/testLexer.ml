open MaoLang
open Parser
open TestCommon

let pp_token fmt = function
  | IDENT id -> Fmt.pf fmt "IDENT %s" id
  | INT i -> Fmt.pf fmt "INT %Ld" i
  | DOUBLE d -> Fmt.pf fmt "DOUBLE %g" d
  | BOOL b -> Fmt.pf fmt "BOOL %b" b
  | CHAR c -> Fmt.pf fmt "CHAR %C" c
  | STRING s -> Fmt.pf fmt "STRING %S" s
  | _ -> Fmt.string fmt "<token>"

let token = Alcotest.testable pp_token ( = )

let tokens src =
  let lexbuf = lexbuf src in
  let rec go acc =
    match Lexer.read lexbuf with EOF -> List.rev acc | t -> go (t :: acc)
  in
  go []

let lexes src expected = Alcotest.(check (list token)) src expected (tokens src)

let rejects src =
  match tokens src with
  | _ -> Alcotest.failf "%S: no lexer error" src
  | exception Lexer.Error _ -> ()

let error_at src =
  match tokens src with
  | _ -> None
  | exception Lexer.Error Loc.{ at = { start_pos; end_pos } } ->
      Some (start_pos.col, end_pos.col - start_pos.col)

let tests =
  [
    test "keywords, types and identifiers" (fun () ->
        lexes "int x_1 = true; while if else skip new return intx"
          [
            INT_TYPE;
            IDENT "x_1";
            DECLARE;
            BOOL true;
            SEMICOLON;
            WHILE;
            IF;
            ELSE;
            SKIP;
            NEW;
            RETURN;
            IDENT "intx";
          ]);
    test "operators" (fun () ->
        lexes ":= == != <= < >= > && || xor ! + - * / % ? :"
          [
            ASSIGN;
            EQ;
            NEQ;
            LEQ;
            LT;
            GEQ;
            GT;
            AND;
            OR;
            XOR;
            NOT;
            PLUS;
            MINUS;
            TIMES;
            DIV;
            MOD;
            QUESTION_MARK;
            COLON;
          ]);
    test "minus is never part of a number" (fun () ->
        lexes "x-1 -2.5" [ IDENT "x"; MINUS; INT 1L; MINUS; DOUBLE 2.5 ]);
    test "numbers" (fun () ->
        lexes "0 42 9223372036854775807 1.5 2. .25"
          [
            INT 0L;
            INT 42L;
            INT Int64.max_int;
            DOUBLE 1.5;
            DOUBLE 2.0;
            DOUBLE 0.25;
          ];
        rejects "9223372036854775808");
    test "chars and strings" (fun () ->
        lexes {|'a' ':' '"' '\n' '\t' '\0' '\\' '\''|}
          [
            CHAR 'a';
            CHAR ':';
            CHAR '"';
            CHAR '\n';
            CHAR '\t';
            CHAR '\000';
            CHAR '\\';
            CHAR '\'';
          ];
        lexes {|"l'a\n\"b\"" ""|} [ STRING "l'a\n\"b\""; STRING "" ]);
    test "invalid literals" (fun () ->
        List.iter rejects [ {|'\q'|}; "''"; "'ab'"; {|"a\qb"|}; {|"open|} ]);
    test ".length" (fun () ->
        lexes "a.length b . length length"
          [ IDENT "a"; DOT_LENGTH; IDENT "b"; DOT_LENGTH; IDENT "length" ];
        List.iter rejects [ "a.len"; "a." ]);
    test "comments and line numbers" (fun () ->
        let lexbuf = lexbuf "// c\n/* a\nb */ x" in
        Alcotest.check token "token" (IDENT "x") (Lexer.read lexbuf);
        Alcotest.(check int) "line" 3 lexbuf.lex_curr_p.pos_lnum;
        rejects "x /* never closed");
    test "unexpected characters" (fun () -> List.iter rejects [ "@"; "&" ]);
    test "non-ASCII characters" (fun () ->
        List.iter
          (fun (src, at) ->
            Alcotest.(check (option (pair int int)))
              src (Some at) (error_at src))
          [
            ("citt\xc3\xa0", (5, 2));
            ("\"citt\xc3\xa0\"", (6, 2));
            ("'\xc3\xa8'", (2, 2));
            ("1 \xe2\x89\xa4 2", (3, 3));
            ("\x80", (1, 1));
            ("x // citt\xc3\xa0", (10, 2));
            ("/* \xc3\xa8 */", (4, 2));
          ];
        lexes "x // ~@#$`\n/* '\" \\ */" [ IDENT "x" ]);
  ]
