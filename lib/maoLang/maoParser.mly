%{
  open MaoAst
  open Utils
%}

%token <string> IDENT
%token <char> CHAR
%token <int> INT
%token <float> DOUBLE
%token <bool> BOOL

%token INT_TYPE BOOL_TYPE DOUBLE_TYPE CHAR_TYPE

%token SKIP ASSIGN DECLARE IF ELSE WHILE
%token NEW LENGTH RETURN

%token PLUS MINUS TIMES DIV MOD
%token AND OR XOR NOT
%token EQ NEQ LT LEQ GT GEQ

%token DOT COMMA SEMICOLON COLON QUESTION_MARK EOF
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

%left OR
%left AND
%left XOR
%nonassoc NOT
%left EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD

%start <command> program

%%

program:
  | c = commands ; EOF { c }

base_type:
  | INT_TYPE { IntType }
  | BOOL_TYPE { BoolType }
  | DOUBLE_TYPE { DoubleType }
  | CHAR_TYPE { CharType }

composite_type:
  | t = base_type ; LBRACKET ; RBRACKET { Array t }

mao_type:
  | t = base_type { BaseType t }
  | t = composite_type { CompositeType t }

base_val:
  | i = INT { IntVal i |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | d = DOUBLE { DoubleVal d |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | b = BOOL { BoolVal b |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | c = CHAR { CharVal c |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }

typed_var:
  | t = mao_type ; id = IDENT { (t, id) }

atom_expr:
  | id = IDENT
    { Var id |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos}
  | v = base_val
    { Val v |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos}
  | id = IDENT ; LPAREN ; es = separated_list(COMMA, expr) ; RPAREN
    { Call (id, es) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos}
  | id = IDENT ; LBRACKET ; e = expr ; RBRACKET
    { ArrayLoad (id, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | id = IDENT ; DOT ; LENGTH
    { ArrayLength id |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | NEW ; t = base_type ; LBRACKET ; e = expr ; RBRACKET
    { NewArrayDefault (t, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | LBRACKET ; vs = separated_nonempty_list(COMMA, base_val) ; RBRACKET
    { NewArrayLiteral vs |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | t = mao_type ; LPAREN ; e = expr ; RPAREN
    { Cast (t, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | LBRACE ; RETURN ; e = expr ; SEMICOLON ; RBRACE { e }
  | LBRACE ; c = command ; RETURN ; e = expr SEMICOLON ; RBRACE
    { ExprBlock (c, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | LPAREN ; e1 = expr ; QUESTION_MARK ; e2 = expr ; COLON ; e3 = expr ; RPAREN
    { Ternary (e1, e2, e3) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | LPAREN ; e = expr ; RPAREN { e }

expr:
  | e1 = expr ; op = PLUS ; e2 = expr
    { BinOp (e1, Add, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = MINUS ; e2 = expr
    { BinOp (e1, Sub, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = TIMES ; e2 = expr
    { BinOp (e1, Mul, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = DIV ; e2 = expr
    { BinOp (e1, Div, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = MOD ; e2 = expr
    { BinOp (e1, Mod, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = AND ; e2 = expr
    { BinOp (e1, And, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = OR ; e2 = expr
    { BinOp (e1, Or, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = XOR ; e2 = expr
    { BinOp (e1, Xor, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = EQ ; e2 = expr
    { BinOp (e1, Eq, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = NEQ ; e2 = expr
    { BinOp (e1, Neq, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = LT ; e2 = expr
    { BinOp (e1, Lt, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = LEQ ; e2 = expr
    { BinOp (e1, Leq, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = GT ; e2 = expr
    { BinOp (e1, Gt, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | e1 = expr ; op = GEQ ; e2 = expr
    { BinOp (e1, Geq, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | op = NOT ; e = expr
    { UnOp (Not, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | op = MINUS ; e = expr
    { UnOp (Neg, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | a = atom_expr { a }

decl_lhs:
  | d = typed_var { [d] }
  | d = typed_var ; COMMA ; ds = separated_nonempty_list(COMMA, typed_var) { d :: ds }
  | d = typed_var ; COMMA ; ids = separated_nonempty_list(COMMA, IDENT)
    { d :: List.map (fun id -> (fst d, id)) ids }

decl_rhs:
  | { None }
  | DECLARE ; es = separated_nonempty_list(COMMA, expr) { Some es }

commands:
  | c = command { c }
  | c1 = command ; c2 = commands { Seq (c1, c2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }

command:
  | SKIP SEMICOLON { Skip |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos}
  | ds = decl_lhs ; es = decl_rhs ; SEMICOLON
    { Declare (ds, es) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | ids = separated_nonempty_list(COMMA, IDENT) ; ASSIGN ; es = separated_nonempty_list(COMMA, expr) ; SEMICOLON
    { Assign (ids, es) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos}
  | id = IDENT ; LBRACKET ; e1 = expr ; RBRACKET ; ASSIGN ; e2 = expr ; SEMICOLON
    { ArrayStore (id, e1, e2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | IF ; LPAREN ; e = expr ; RPAREN ; LBRACE ; cs1 = commands ; RBRACE ; ELSE ; LBRACE ; cs2 = commands ; RBRACE
    { IfThenElse (e, cs1, cs2) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | WHILE ; LPAREN ; e = expr ; RPAREN ; LBRACE ; cs = commands RBRACE
    { While (e, cs) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | LBRACE ; cs = commands ; RBRACE
    { Block cs |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | id = IDENT ; LPAREN ; RPAREN ; LBRACE ; cs = commands ; RBRACE
    { Proc (id, [], cs) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | id = IDENT ; LPAREN ; RPAREN ; SEMICOLON
    { Invoke (id, []) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | id = IDENT ; LPAREN ; ds = separated_nonempty_list(COMMA, typed_var) ; RPAREN ; LBRACE ; cs = commands ; RBRACE
    { Proc (id, ds, cs) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | id = IDENT ; LPAREN ; es = separated_nonempty_list(COMMA, expr) ; RPAREN ; SEMICOLON
    { Invoke (id, es) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
  | t = mao_type ; id = IDENT ; LPAREN ; ds = separated_list(COMMA, typed_var) ; RPAREN ; e = expr
    { Func (id, ds, t, e) |> Loc.with_positions ~start_pos:$startpos ~end_pos:$endpos }
