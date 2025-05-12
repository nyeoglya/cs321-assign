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

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
