type token =
  | VAR of (string)
  | LPAREN
  | RPAREN
  | EOF
  | LET
  | IN
  | EQ
  | DOT
  | LAM

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Uml.exp
