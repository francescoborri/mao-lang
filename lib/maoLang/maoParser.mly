%{
  open MaoAst
  open Utils

  let unpack_string (start_pos, end_pos) =
    String.to_seq >> List.of_seq
    >> List.map (fun c -> Loc.with_positions ~start_pos ~end_pos (CharVal c))

  let l (start_pos, end_pos) x = Loc.with_positions ~start_pos ~end_pos x
%}

%token <string> IDENT
%token <Int64.t> INT
%token <float> DOUBLE
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token INT_TYPE BOOL_TYPE DOUBLE_TYPE CHAR_TYPE
%token SKIP ASSIGN DECLARE IF ELSE WHILE
%token NEW RETURN
%token PLUS MINUS TIMES DIV MOD
%token AND OR XOR NOT
%token EQ NEQ LT LEQ GT GEQ
%token DOT_LENGTH COMMA SEMICOLON COLON QUESTION_MARK EOF
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

%start <command> program

%left OR
%left AND
%left XOR
%nonassoc NOT
%left EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc UMINUS

%%

program:
| c = commands EOF { c }

base_type:
| INT_TYPE { IntType }
| BOOL_TYPE { BoolType }
| DOUBLE_TYPE { DoubleType }
| CHAR_TYPE { CharType }

composite_type:
| t = base_type LBRACKET RBRACKET { Array t }

mao_type:
| t = base_type { BaseType t }
| t = composite_type { CompositeType t }

base_val:
| i = INT { l $loc @@ IntVal i }
| d = DOUBLE { l $loc @@ DoubleVal d }
| b = BOOL { l $loc @@ BoolVal b }
| c = CHAR { l $loc @@ CharVal c }

signed_val:
| v = base_val { v }
| MINUS i = INT { l $loc @@ IntVal (Int64.neg i) }
| MINUS d = DOUBLE { l $loc @@ DoubleVal (-.d) }

typed_var:
| t = mao_type id = IDENT { t, id }

atom_expr:
| id = IDENT { l $loc @@ Var id }
| v = base_val { l $loc @@ Val v }
| id = IDENT LPAREN es = separated_list(COMMA, expr) RPAREN { l $loc @@ Call (id, es) }
| id = IDENT LBRACKET e = expr RBRACKET { l $loc @@ ArrayLoad (id, e) }
| id = IDENT DOT_LENGTH { l $loc @@ ArrayLength id }
| NEW t = base_type LBRACKET e = expr RBRACKET { l $loc @@ NewArrayDefault (t, e) }
| LBRACKET vs = separated_nonempty_list(COMMA, signed_val) RBRACKET { l $loc @@ NewArrayLiteral vs }
| s = STRING { l $loc @@ NewArrayLiteral (unpack_string $loc s) }
| t = mao_type LPAREN e = expr RPAREN { l $loc @@ Cast (t, e) }
| LBRACE RETURN e = expr SEMICOLON RBRACE { e }
| LBRACE cs = commands RETURN e = expr SEMICOLON RBRACE { l $loc @@ ExprBlock (cs, e) }
| LPAREN e1 = expr QUESTION_MARK e2 = expr COLON e3 = expr RPAREN { l $loc @@ Ternary (e1, e2, e3) }
| LPAREN e = expr RPAREN { e }

%inline bin_op:
| PLUS { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV { Div }
| MOD { Mod }
| AND { And }
| OR { Or }
| XOR { Xor }
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LEQ { Leq }
| GT { Gt }
| GEQ { Geq }

expr:
| e1 = expr op = bin_op e2 = expr { l $loc @@ BinOp (e1, op, e2) }
| NOT e = expr { l $loc @@ UnOp (Not, e) }
| MINUS e = expr %prec UMINUS { l $loc @@ UnOp (Neg, e) }
| a = atom_expr { a }

decl_lhs:
| d = typed_var { [ d ] }
| d = typed_var COMMA ds = separated_nonempty_list(COMMA, typed_var) { d :: ds }
| d = typed_var COMMA ids = separated_nonempty_list(COMMA, IDENT)
  { d :: List.map (fun id -> (fst d, id)) ids }

decl_rhs:
| { None }
| DECLARE es = separated_nonempty_list(COMMA, expr) { Some es }

commands:
| c = command { c }
| c1 = command c2 = commands { l $loc @@ Seq (c1, c2) }

command:
| SKIP SEMICOLON { l $loc @@ Skip }
| ds = decl_lhs es = decl_rhs SEMICOLON { l $loc @@ Declare (ds, es) }
| ids = separated_nonempty_list(COMMA, IDENT) ASSIGN es = separated_nonempty_list(COMMA, expr) SEMICOLON
  { l $loc @@ Assign (ids, es) }
| id = IDENT LBRACKET e1 = expr RBRACKET ASSIGN e2 = expr SEMICOLON
  { l $loc @@ ArrayStore (id, e1, e2) }
| IF LPAREN e = expr RPAREN LBRACE cs1 = commands RBRACE ELSE LBRACE cs2 = commands RBRACE
  { l $loc @@ IfThenElse (e, cs1, cs2) }
| IF LPAREN e = expr RPAREN LBRACE cs = commands RBRACE { l $loc @@ IfThenElse (e, cs, l $loc Skip) }
| WHILE LPAREN e = expr RPAREN LBRACE cs = commands RBRACE { l $loc @@ While (e, cs) }
| LBRACE cs = commands RBRACE { l $loc @@ Block cs }
| id = IDENT LPAREN RPAREN LBRACE cs = commands RBRACE { l $loc @@ Proc (id, [], cs) }
| id = IDENT LPAREN RPAREN SEMICOLON { l $loc @@ Invoke (id, []) }
| id = IDENT LPAREN ds = decl_lhs RPAREN LBRACE cs = commands RBRACE { l $loc @@ Proc (id, ds, cs) }
| id = IDENT LPAREN es = separated_nonempty_list(COMMA, expr) RPAREN SEMICOLON
  { l $loc @@ Invoke (id, es) }
| t = mao_type id = IDENT LPAREN RPAREN e = expr { l $loc @@ Func (id, [], t, e) }
| t = mao_type id = IDENT LPAREN ds = decl_lhs RPAREN e = expr { l $loc @@ Func (id, ds, t, e) }
