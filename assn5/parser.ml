type token =
  | VAR of (string)
  | NUM of (int)
  | LPAREN
  | RPAREN
  | EOF
  | LET
  | IN
  | EQ
  | COMMA
  | FN
  | COLON
  | BOOL
  | INT
  | UNIT
  | PLUS
  | PROD
  | ARROW
  | FST
  | SND
  | CASE
  | INL
  | INR
  | EUNIT
  | FIX
  | REC
  | MINUS
  | DOUBLEARROW
  | OF
  | OR
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
# 41 "parser.ml"
let yytransl_const = [|
  259 (* LPAREN *);
  260 (* RPAREN *);
    0 (* EOF *);
  261 (* LET *);
  262 (* IN *);
  263 (* EQ *);
  264 (* COMMA *);
  265 (* FN *);
  266 (* COLON *);
  267 (* BOOL *);
  268 (* INT *);
  269 (* UNIT *);
  270 (* PLUS *);
  271 (* PROD *);
  272 (* ARROW *);
  273 (* FST *);
  274 (* SND *);
  275 (* CASE *);
  276 (* INL *);
  277 (* INR *);
  278 (* EUNIT *);
  279 (* FIX *);
  280 (* REC *);
  281 (* MINUS *);
  282 (* DOUBLEARROW *);
  283 (* OF *);
  284 (* OR *);
  285 (* TRUE *);
  286 (* FALSE *);
  287 (* IF *);
  288 (* THEN *);
  289 (* ELSE *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* NUM *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\001\000\
\003\000\003\000\004\000\004\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\001\000\003\000\003\000\003\000\002\000\
\001\000\006\000\001\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\005\000\002\000\002\000\005\000\005\000\012\000\
\006\000\006\000\001\000\001\000\001\000\008\000\009\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\014\000\018\000\000\000\000\000\029\000\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\028\000\016\000\017\000\000\000\032\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\020\000\021\000\000\000\000\000\
\000\000\000\000\000\000\008\000\012\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\003\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\001\000\022\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\000\000\010\000\
\000\000\025\000\026\000\000\000\000\000\000\000\030\000\000\000\
\000\000\031\000\000\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\021\000\048\000\022\000\023\000\024\000"

let yysindex = "\012\000\
\007\255\000\000\000\000\000\000\007\255\016\255\000\000\021\255\
\000\000\007\255\007\255\007\255\020\255\030\255\000\000\042\255\
\000\000\000\000\000\000\007\255\000\000\039\000\079\255\000\000\
\027\255\039\255\054\255\046\255\000\000\000\000\032\255\126\255\
\126\255\050\255\033\255\000\000\000\000\000\000\007\255\126\255\
\060\255\126\255\055\255\126\255\000\000\000\000\000\000\091\255\
\099\255\126\255\007\255\073\255\004\255\126\255\102\255\077\255\
\107\255\007\255\126\255\126\255\126\255\007\255\110\255\052\255\
\000\000\007\255\127\255\007\255\061\255\000\000\000\000\244\254\
\244\254\244\254\000\000\007\255\007\255\083\255\007\255\000\000\
\007\255\000\000\000\000\007\255\084\255\063\255\000\000\007\255\
\071\255\000\000\093\255\086\255\007\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\255\
\040\255\047\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\008\000\251\255\000\000\096\000"

let yytablesize = 290
let yytable = "\025\000\
\009\000\059\000\060\000\061\000\029\000\030\000\031\000\003\000\
\004\000\005\000\066\000\006\000\001\000\007\000\035\000\008\000\
\026\000\059\000\060\000\061\000\009\000\028\000\032\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\038\000\017\000\
\033\000\052\000\039\000\018\000\019\000\020\000\036\000\027\000\
\049\000\007\000\034\000\006\000\007\000\064\000\006\000\053\000\
\040\000\055\000\005\000\057\000\071\000\005\000\041\000\042\000\
\075\000\063\000\043\000\050\000\078\000\067\000\080\000\007\000\
\051\000\006\000\072\000\073\000\074\000\054\000\082\000\083\000\
\005\000\085\000\056\000\086\000\065\000\069\000\087\000\003\000\
\004\000\005\000\090\000\006\000\077\000\007\000\081\000\094\000\
\084\000\088\000\089\000\091\000\009\000\092\000\058\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\062\000\017\000\
\059\000\060\000\061\000\018\000\019\000\020\000\070\000\093\000\
\059\000\060\000\061\000\059\000\060\000\061\000\037\000\000\000\
\059\000\060\000\061\000\059\000\060\000\061\000\000\000\068\000\
\044\000\000\000\000\000\000\000\000\000\079\000\000\000\076\000\
\045\000\046\000\047\000\000\000\059\000\060\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\009\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\009\000\000\000\000\000\000\000\
\009\000\009\000"

let yycheck = "\005\000\
\000\000\014\001\015\001\016\001\010\000\011\000\012\000\001\001\
\002\001\003\001\007\001\005\001\001\000\007\001\020\000\009\001\
\001\001\014\001\015\001\016\001\014\001\001\001\003\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\004\001\025\001\
\003\001\039\000\008\001\029\001\030\001\031\001\000\000\024\001\
\033\000\004\001\001\001\004\001\007\001\051\000\007\001\040\000\
\010\001\042\000\004\001\044\000\058\000\007\001\001\001\010\001\
\062\000\050\000\027\001\010\001\066\000\054\000\068\000\026\001\
\032\001\026\001\059\000\060\000\061\000\010\001\076\000\077\000\
\026\001\079\000\020\001\081\000\004\001\001\001\084\000\001\001\
\002\001\003\001\088\000\005\001\033\001\007\001\026\001\093\000\
\006\001\006\001\028\001\021\001\014\001\001\001\004\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\004\001\025\001\
\014\001\015\001\016\001\029\001\030\001\031\001\004\001\026\001\
\014\001\015\001\016\001\014\001\015\001\016\001\023\000\255\255\
\014\001\015\001\016\001\014\001\015\001\016\001\255\255\026\001\
\003\001\255\255\255\255\255\255\255\255\007\001\255\255\026\001\
\011\001\012\001\013\001\255\255\014\001\015\001\016\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\006\001\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\028\001\255\255\255\255\255\255\
\032\001\033\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  LET\000\
  IN\000\
  EQ\000\
  COMMA\000\
  FN\000\
  COLON\000\
  BOOL\000\
  INT\000\
  UNIT\000\
  PLUS\000\
  PROD\000\
  ARROW\000\
  FST\000\
  SND\000\
  CASE\000\
  INL\000\
  INR\000\
  EUNIT\000\
  FIX\000\
  REC\000\
  MINUS\000\
  DOUBLEARROW\000\
  OF\000\
  OR\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  "

let yynames_block = "\
  VAR\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tml.tp) in
    Obj.repr(
# 14 "parser.mly"
                            ( _2 )
# 268 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    Obj.repr(
# 15 "parser.mly"
                            ( Tml.Bool )
# 274 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    Obj.repr(
# 16 "parser.mly"
                            ( Tml.Int )
# 280 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
                            ( Tml.Unit )
# 286 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.tp) in
    Obj.repr(
# 18 "parser.mly"
                            ( Tml.Fun (_1, _3) )
# 294 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.tp) in
    Obj.repr(
# 19 "parser.mly"
                            ( Tml.Prod (_1, _3) )
# 302 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tml.tp) in
    Obj.repr(
# 20 "parser.mly"
                            ( Tml.Sum (_1, _3) )
# 310 "parser.ml"
               : Tml.tp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 24 "parser.mly"
                            ( _1 )
# 317 "parser.ml"
               : Tml.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appterm) in
    Obj.repr(
# 28 "parser.mly"
                                                                     ( _1 )
# 324 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 29 "parser.mly"
                                                                     ( Tml.Lam (_2, _4, _6) )
# 333 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aterm) in
    Obj.repr(
# 33 "parser.mly"
                            ( _1 )
# 340 "parser.ml"
               : 'appterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appterm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aterm) in
    Obj.repr(
# 34 "parser.mly"
                            ( Tml.App (_1, _2) )
# 348 "parser.ml"
               : 'appterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 37 "parser.mly"
                                                                     ( _2 )
# 355 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                                                                     ( Tml.Var _1 )
# 362 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                                                                     ( Tml.Eunit )
# 368 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                                                     ( Tml.True )
# 374 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                                                                     ( Tml.False )
# 380 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                                                     ( Tml.Num _1 )
# 387 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 43 "parser.mly"
                                                                     ( Tml.Pair (_2, _4) )
# 395 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "parser.mly"
                                                                     ( Tml.Fst _2 )
# 402 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 45 "parser.mly"
                                                                     ( Tml.Snd _2 )
# 409 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 46 "parser.mly"
                                                                     ( Tml.Inl (_5, _3) )
# 417 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 47 "parser.mly"
                                                                     ( Tml.Inr (_5, _3) )
# 425 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'term) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _12 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "parser.mly"
                                                                     ( Tml.Case (_2, _5, _7, _10, _12) )
# 436 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Tml.tp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 49 "parser.mly"
                                                                     ( Tml.Fix (_2, _4, _6) )
# 445 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 50 "parser.mly"
                                                                     ( Tml.Ifthenelse (_2, _4, _6) )
# 454 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                                                                     ( Tml.Plus )
# 460 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                                                                     ( Tml.Minus )
# 466 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                                                                     ( Tml.Eq )
# 472 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Tml.tp) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
                                                                     ( Tml.App (Tml.Lam (_2, _4, _8), _6) )
# 482 "parser.ml"
               : 'aterm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : Tml.tp) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 55 "parser.mly"
                                                                     ( Tml.App (Tml.Lam (_3, _5, _9), Tml.Fix (_3, _5, _7)) )
# 492 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tml.exp)
