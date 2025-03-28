{
open Parser 
}
rule token = parse
  | [' ''\t''\n']                        { token lexbuf }
  | '('                                  { LPAREN }
  | ')'                                  { RPAREN }
  | '='                                  { EQ }
  | "let"                                { LET }
  | "in"                                 { IN }
  | "lam"                                { LAM }
  | '.'                                  { DOT }
  | ['A'-'Z''a'-'z''0'-'9''_''\'']+ as s     { VAR(s) }
  | ';'                                  { EOF }
  
