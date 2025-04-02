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

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tml.exp
