type token =
  | VAR of (string)
  | NUM of (int)
  | TRUE of (bool)
  | FALSE of (bool)
  | LPAREN
  | RPAREN
  | INT
  | BOOL
  | UNIT
  | EUNIT
  | PLUS
  | MINUS
  | PROD
  | EQ
  | NEQ
  | ARROW
  | DOUBLEARROW
  | COLON
  | UNDERSCORE
  | FN
  | LET
  | IN
  | END
  | OF
  | VAL
  | REC
  | DATATYPE
  | COMMA
  | OR
  | ENDDEC
  | ENDEXP

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
# 38 "parser.ml"
let yytransl_const = [|
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* INT *);
  264 (* BOOL *);
  265 (* UNIT *);
  266 (* EUNIT *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* PROD *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* ARROW *);
  273 (* DOUBLEARROW *);
  274 (* COLON *);
  275 (* UNDERSCORE *);
  276 (* FN *);
  277 (* LET *);
  278 (* IN *);
  279 (* END *);
  280 (* OF *);
  281 (* VAL *);
  282 (* REC *);
  283 (* DATATYPE *);
  284 (* COMMA *);
  285 (* OR *);
  286 (* ENDDEC *);
  287 (* ENDEXP *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* NUM *);
  259 (* TRUE *);
  260 (* FALSE *);
    0|]

let yylhs = "\255\255\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\005\000\008\000\008\000\007\000\009\000\
\009\000\006\000\006\000\006\000\006\000\010\000\010\000\010\000\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\011\000\011\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\003\000\
\003\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\004\000\005\000\004\000\001\000\003\000\004\000\
\002\000\004\000\001\000\003\000\003\000\003\000\003\000\003\000\
\005\000\005\000\002\000\001\000\002\000\003\000\005\000\001\000\
\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\052\000\048\000\049\000\050\000\000\000\051\000\
\000\000\000\000\000\000\000\000\053\000\000\000\030\000\000\000\
\000\000\044\000\000\000\000\000\000\000\010\000\011\000\012\000\
\000\000\013\000\009\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\045\000\046\000\000\000\000\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\002\000\003\000\004\000\
\000\000\000\000\008\000\000\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\034\000\032\000\
\000\000\042\000\000\000\000\000\047\000\041\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\018\000\022\000"

let yydgoto = "\002\000\
\013\000\019\000\073\000\028\000\085\000\020\000\029\000\086\000\
\030\000\016\000\017\000\018\000"

let yysindex = "\011\000\
\006\255\000\000\000\000\000\000\000\000\000\000\006\255\000\000\
\051\000\253\254\173\255\013\255\000\000\026\000\000\000\254\254\
\075\000\000\000\238\255\026\255\051\000\000\000\000\000\000\000\
\051\000\000\000\000\000\053\255\005\255\000\000\253\254\040\255\
\051\000\035\255\059\255\025\000\025\000\025\000\025\000\025\000\
\000\000\006\255\025\000\000\000\000\000\057\000\025\000\000\000\
\001\255\012\255\025\000\057\000\051\000\051\000\025\000\037\255\
\025\000\000\255\025\000\139\255\139\255\052\255\081\000\081\000\
\036\000\057\255\249\255\000\000\057\000\000\000\000\000\000\000\
\066\255\076\000\000\000\081\000\007\255\001\255\000\000\060\000\
\025\000\081\000\065\255\000\255\062\255\000\000\000\000\000\000\
\220\255\000\000\057\000\057\000\000\000\000\000\081\000\057\000\
\087\255\000\255\000\000\080\255\007\255\007\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\046\255\000\000\000\000\000\000\251\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\072\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\121\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\100\255\114\255\086\255\166\255\180\255\
\000\000\071\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\194\255\253\255\153\255\000\000\000\000\
\000\000\118\255\184\255\000\000\212\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\215\255\000\000\
\000\000\000\000\000\000\133\255\151\255\198\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\208\255\248\255\021\000\005\000\000\000\009\000\
\056\000\000\000\000\000\096\000"

let yytablesize = 352
let yytable = "\014\000\
\083\000\031\000\034\000\077\000\084\000\015\000\003\000\004\000\
\005\000\006\000\007\000\001\000\049\000\035\000\032\000\008\000\
\050\000\075\000\052\000\091\000\089\000\011\000\092\000\012\000\
\056\000\009\000\010\000\042\000\053\000\052\000\011\000\048\000\
\012\000\054\000\060\000\061\000\062\000\063\000\064\000\053\000\
\065\000\067\000\100\000\101\000\078\000\074\000\066\000\102\000\
\057\000\076\000\081\000\035\000\052\000\080\000\052\000\082\000\
\035\000\035\000\035\000\035\000\035\000\055\000\053\000\035\000\
\053\000\039\000\040\000\035\000\035\000\051\000\052\000\090\000\
\058\000\035\000\035\000\035\000\035\000\024\000\091\000\095\000\
\053\000\092\000\024\000\024\000\024\000\024\000\024\000\088\000\
\096\000\024\000\098\000\038\000\103\000\024\000\024\000\092\000\
\038\000\038\000\038\000\024\000\031\000\024\000\024\000\038\000\
\097\000\036\000\104\000\038\000\038\000\079\000\036\000\036\000\
\044\000\038\000\038\000\038\000\038\000\036\000\000\000\037\000\
\000\000\036\000\036\000\027\000\037\000\037\000\015\000\036\000\
\036\000\036\000\036\000\037\000\000\000\000\000\015\000\037\000\
\037\000\015\000\007\000\027\000\000\000\037\000\037\000\037\000\
\037\000\007\000\007\000\027\000\027\000\007\000\007\000\038\000\
\039\000\040\000\007\000\000\000\006\000\000\000\016\000\000\000\
\007\000\007\000\007\000\007\000\006\000\000\000\016\000\006\000\
\006\000\016\000\000\000\039\000\006\000\021\000\022\000\023\000\
\024\000\025\000\006\000\006\000\006\000\006\000\026\000\039\000\
\000\000\040\000\000\000\039\000\039\000\019\000\000\000\027\000\
\000\000\039\000\039\000\039\000\039\000\040\000\033\000\023\000\
\000\000\040\000\040\000\020\000\000\000\019\000\000\000\040\000\
\040\000\040\000\040\000\023\000\019\000\019\000\019\000\023\000\
\023\000\021\000\000\000\020\000\028\000\023\000\023\000\023\000\
\023\000\099\000\020\000\020\000\020\000\000\000\000\000\000\000\
\091\000\021\000\000\000\092\000\028\000\000\000\000\000\000\000\
\000\000\021\000\021\000\045\000\028\000\028\000\000\000\000\000\
\036\000\037\000\038\000\039\000\040\000\000\000\045\000\046\000\
\014\000\000\000\017\000\036\000\037\000\038\000\039\000\040\000\
\014\000\047\000\017\000\014\000\014\000\017\000\017\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\014\000\000\000\
\017\000\003\000\004\000\005\000\006\000\059\000\000\000\000\000\
\000\000\000\000\008\000\000\000\036\000\037\000\038\000\039\000\
\040\000\000\000\000\000\000\000\009\000\010\000\036\000\037\000\
\038\000\039\000\040\000\021\000\022\000\023\000\024\000\025\000\
\041\000\068\000\000\000\000\000\026\000\069\000\000\000\070\000\
\071\000\072\000\087\000\000\000\000\000\027\000\036\000\037\000\
\038\000\039\000\040\000\003\000\004\000\005\000\006\000\043\000\
\000\000\093\000\094\000\000\000\008\000\000\000\036\000\037\000\
\038\000\039\000\040\000\036\000\037\000\038\000\039\000\040\000"

let yycheck = "\001\000\
\001\001\005\001\011\000\052\000\005\001\001\000\001\001\002\001\
\003\001\004\001\005\001\001\000\021\000\001\001\010\000\010\001\
\025\000\006\001\018\001\013\001\069\000\025\001\016\001\027\001\
\033\000\020\001\021\001\030\001\028\001\018\001\025\001\006\001\
\027\001\029\001\036\000\037\000\038\000\039\000\040\000\028\001\
\042\000\043\000\091\000\092\000\053\000\047\000\042\000\096\000\
\014\001\051\000\014\001\006\001\018\001\055\000\018\001\057\000\
\011\001\012\001\013\001\014\001\015\001\022\001\028\001\018\001\
\028\001\014\001\015\001\022\001\023\001\017\001\018\001\006\001\
\014\001\028\001\029\001\030\001\031\001\006\001\013\001\081\000\
\028\001\016\001\011\001\012\001\013\001\014\001\015\001\031\001\
\024\001\018\001\029\001\006\001\006\001\022\001\023\001\016\001\
\011\001\012\001\013\001\028\001\030\001\030\001\031\001\018\001\
\084\000\006\001\098\000\022\001\023\001\054\000\011\001\012\001\
\017\000\028\001\029\001\030\001\031\001\018\001\255\255\006\001\
\255\255\022\001\023\001\006\001\011\001\012\001\006\001\028\001\
\029\001\030\001\031\001\018\001\255\255\255\255\014\001\022\001\
\023\001\017\001\006\001\022\001\255\255\028\001\029\001\030\001\
\031\001\013\001\014\001\030\001\031\001\017\001\018\001\013\001\
\014\001\015\001\022\001\255\255\006\001\255\255\006\001\255\255\
\028\001\029\001\030\001\031\001\014\001\255\255\014\001\017\001\
\018\001\017\001\255\255\006\001\022\001\001\001\002\001\003\001\
\004\001\005\001\028\001\029\001\030\001\031\001\010\001\018\001\
\255\255\006\001\255\255\022\001\023\001\006\001\255\255\019\001\
\255\255\028\001\029\001\030\001\031\001\018\001\026\001\006\001\
\255\255\022\001\023\001\006\001\255\255\022\001\255\255\028\001\
\029\001\030\001\031\001\018\001\029\001\030\001\031\001\022\001\
\023\001\006\001\255\255\022\001\006\001\028\001\029\001\030\001\
\031\001\006\001\029\001\030\001\031\001\255\255\255\255\255\255\
\013\001\022\001\255\255\016\001\022\001\255\255\255\255\255\255\
\255\255\030\001\031\001\006\001\030\001\031\001\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\255\255\006\001\018\001\
\006\001\255\255\006\001\011\001\012\001\013\001\014\001\015\001\
\014\001\028\001\014\001\017\001\018\001\017\001\018\001\255\255\
\255\255\255\255\255\255\255\255\028\001\255\255\028\001\255\255\
\028\001\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\255\255\010\001\255\255\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\020\001\021\001\011\001\012\001\
\013\001\014\001\015\001\001\001\002\001\003\001\004\001\005\001\
\031\001\001\001\255\255\255\255\010\001\005\001\255\255\007\001\
\008\001\009\001\031\001\255\255\255\255\019\001\011\001\012\001\
\013\001\014\001\015\001\001\001\002\001\003\001\004\001\005\001\
\255\255\006\001\023\001\255\255\010\001\255\255\011\001\012\001\
\013\001\014\001\015\001\011\001\012\001\013\001\014\001\015\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  INT\000\
  BOOL\000\
  UNIT\000\
  EUNIT\000\
  PLUS\000\
  MINUS\000\
  PROD\000\
  EQ\000\
  NEQ\000\
  ARROW\000\
  DOUBLEARROW\000\
  COLON\000\
  UNDERSCORE\000\
  FN\000\
  LET\000\
  IN\000\
  END\000\
  OF\000\
  VAL\000\
  REC\000\
  DATATYPE\000\
  COMMA\000\
  OR\000\
  ENDDEC\000\
  ENDEXP\000\
  "

let yynames_block = "\
  VAR\000\
  NUM\000\
  TRUE\000\
  FALSE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.ty) in
    Obj.repr(
# 24 "parser.mly"
                            ( _2 )
# 284 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
                            ( Ast.T_INT )
# 290 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
                            ( Ast.T_BOOL )
# 296 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
                            ( Ast.T_UNIT )
# 302 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 28 "parser.mly"
                            ( Ast.T_CON _1 )
# 309 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ty) in
    Obj.repr(
# 29 "parser.mly"
                            ( Ast.T_FUN (_1, _3) )
# 317 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ty) in
    Obj.repr(
# 30 "parser.mly"
                            ( Ast.T_PAIR (_1, _3) )
# 325 "parser.ml"
               : Ast.ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.pat) in
    Obj.repr(
# 33 "parser.mly"
                            ( _2 )
# 332 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
                            ( Ast.P_WILD )
# 338 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 35 "parser.mly"
                            ( Ast.P_INT _1 )
# 345 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 36 "parser.mly"
                            ( Ast.P_BOOL _1 )
# 352 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 37 "parser.mly"
                            ( Ast.P_BOOL _1 )
# 359 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                            ( Ast.P_UNIT )
# 365 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                            ( Ast.P_VID _1 )
# 372 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.pat) in
    Obj.repr(
# 40 "parser.mly"
                            ( Ast.P_VIDP (_1, _2) )
# 380 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.pat) in
    Obj.repr(
# 41 "parser.mly"
                            ( Ast.P_PAIR (_1, _3) )
# 388 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ty) in
    Obj.repr(
# 42 "parser.mly"
                            ( Ast.P_TPAT (_1, _3) )
# 396 "parser.ml"
               : Ast.pat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.conbinding) in
    Obj.repr(
# 45 "parser.mly"
                            ( _2 )
# 403 "parser.ml"
               : Ast.conbinding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
                            ( Ast.CB_VID _1 )
# 410 "parser.ml"
               : Ast.conbinding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.ty) in
    Obj.repr(
# 47 "parser.mly"
                            ( Ast.CB_TVID (_1, _3) )
# 418 "parser.ml"
               : Ast.conbinding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.conbinding) in
    Obj.repr(
# 50 "parser.mly"
                            ( [_1] )
# 425 "parser.ml"
               : 'conbind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.conbinding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'conbind) in
    Obj.repr(
# 51 "parser.mly"
                            ( [_1] @ _3 )
# 433 "parser.ml"
               : 'conbind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 54 "parser.mly"
                            ( Ast.M_RULE (_1, _3) )
# 441 "parser.ml"
               : Ast.mrule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.mrule) in
    Obj.repr(
# 57 "parser.mly"
                            ( [_1] )
# 448 "parser.ml"
               : 'mlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.mrule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mlist) in
    Obj.repr(
# 58 "parser.mly"
                            ( [_1] @ _3 )
# 456 "parser.ml"
               : 'mlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.dec) in
    Obj.repr(
# 61 "parser.mly"
                            ( _2 )
# 463 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.pat) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 62 "parser.mly"
                            ( Ast.D_VAL (_2, _4) )
# 471 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.pat) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 63 "parser.mly"
                            ( Ast.D_REC (_3, _5) )
# 479 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'conbind) in
    Obj.repr(
# 64 "parser.mly"
                            ( Ast.D_DTYPE (_2, _4) )
# 487 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.dec) in
    Obj.repr(
# 67 "parser.mly"
                                 ( [_1] )
# 494 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'declist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.dec) in
    Obj.repr(
# 68 "parser.mly"
                                 ( _1 @ [_3] )
# 502 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.dec) in
    Obj.repr(
# 69 "parser.mly"
                                 ( _1 )
# 510 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 72 "parser.mly"
                                 ( ([], _1) )
# 517 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 73 "parser.mly"
                                 ( (_1, _3) )
# 525 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appterm) in
    Obj.repr(
# 76 "parser.mly"
                                       ( _1 )
# 532 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 77 "parser.mly"
                                       ( Ast.E_APP (Ast.E_PLUS, Ast.E_PAIR (_1, _3)) )
# 540 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 78 "parser.mly"
                                       ( Ast.E_APP (Ast.E_MINUS, Ast.E_PAIR (_1, _3)) )
# 548 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 79 "parser.mly"
                                       ( Ast.E_APP (Ast.E_MULT, Ast.E_PAIR (_1, _3)) )
# 556 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 80 "parser.mly"
                                       ( Ast.E_APP (Ast.E_EQ, Ast.E_PAIR (_1, _3)) )
# 564 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 81 "parser.mly"
                                       ( Ast.E_APP (Ast.E_NEQ, Ast.E_PAIR (_1, _3)) )
# 572 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 82 "parser.mly"
                                       ( Ast.E_LET (_2, _4) )
# 580 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.ty) in
    Obj.repr(
# 83 "parser.mly"
                                       ( Ast.E_TEXP (_2, _4) )
# 588 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mlist) in
    Obj.repr(
# 84 "parser.mly"
                                       ( Ast.E_FUN _2 )
# 595 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aterm) in
    Obj.repr(
# 87 "parser.mly"
                                       ( _1 )
# 602 "parser.ml"
               : 'appterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appterm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aterm) in
    Obj.repr(
# 88 "parser.mly"
                                       ( Ast.E_APP (_1, _2) )
# 610 "parser.ml"
               : 'appterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 91 "parser.mly"
                                       ( _2 )
# 617 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 92 "parser.mly"
                                       ( Ast.E_PAIR (_2, _4) )
# 625 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "parser.mly"
                                       ( Ast.E_INT _1 )
# 632 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 94 "parser.mly"
                                       ( Ast.E_BOOL _1 )
# 639 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 95 "parser.mly"
                                       ( Ast.E_BOOL _1 )
# 646 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                       ( Ast.E_UNIT )
# 652 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                                       ( Ast.E_VID _1 )
# 659 "parser.ml"
               : 'aterm))
(* Entry parse *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
