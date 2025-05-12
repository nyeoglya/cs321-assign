{
open Parser 
}
rule token = parse
  | [' ''\t''\n']                                       { token lexbuf }
  | '('                                                 { LPAREN }
  | ')'                                                 { RPAREN }
  | "int"                                               { INT }
  | "bool"                                              { BOOL } 
  | "unit"                                              { UNIT }
  | ('~'?['0'-'9']+) as i                               { let s = 
                                                           if i.[0] = '~' 
                                                           then 
                                                            "-"^(String.sub i 1 ((String.length i) -1))
                                                           else i
                                                          in NUM(int_of_string s) }
  | "true" as b                                         { TRUE(bool_of_string b) }
  | "false" as b                                        { FALSE(bool_of_string b) }
  | "()"                                                { EUNIT }
  | '+'                                                 { PLUS }
  | '-'                                                 { MINUS }
  | '*'                                                 { PROD }
  | '='                                                 { EQ }
  | "<>"                                                { NEQ }
  | "->"                                                { ARROW }
  | "=>"                                                { DOUBLEARROW }
  | ':'                                                 { COLON }
  | '_'                                                 { UNDERSCORE }
  | "fn"                                                { FN }
  | "let"                                               { LET }
  | "in"                                                { IN }
  | "end"                                               { END }
  | "of"                                                { OF }
  | "val"                                               { VAL }
  | "rec"                                               { REC }
  | "datatype"                                          { DATATYPE }
  | ','                                                 { COMMA }  
  | '|'                                                 { OR }
  | ['A'-'Z''a'-'z''\''] ['A'-'Z''a'-'z''0'-'9''_''\'']* as s  { VAR(s) }
  | ";;"                                                { ENDEXP }
  | ";"                                                 { ENDDEC }
