%{
%}

%token <string> VAR
%token <int> NUM
%token LPAREN RPAREN EOF LET IN EQ COMMA FN COLON BOOL INT UNIT PLUS PROD ARROW FST SND CASE INL INR EUNIT FIX REC MINUS DOUBLEARROW OF OR TRUE FALSE IF THEN ELSE

%start parse
%type <Tml.exp> parse
%type <Tml.tp> tp
%%

tp:
| LPAREN tp RPAREN          { $2 }
| BOOL                      { Tml.Bool }
| INT                       { Tml.Int }
| UNIT                      { Tml.Unit }
| tp ARROW tp               { Tml.Fun ($1, $3) }
| tp PROD tp                { Tml.Prod ($1, $3) }
| tp PLUS tp                { Tml.Sum ($1, $3) }
;

parse:
| term EOF                  { $1 }
;

term:
| appterm                                                            { $1 }         
| FN VAR COLON tp DOUBLEARROW term                                   { Tml.Lam ($2, $4, $6) }
;

appterm:
| aterm                     { $1 }
| appterm aterm             { Tml.App ($1, $2) }

aterm:
| LPAREN term RPAREN                                                 { $2 }
| VAR                                                                { Tml.Var $1 }
| EUNIT                                                              { Tml.Eunit } 
| TRUE                                                               { Tml.True }                           
| FALSE                                                              { Tml.False } 
| NUM                                                                { Tml.Num $1 } 
| LPAREN term COMMA term RPAREN                                      { Tml.Pair ($2, $4) }                          
| FST term                                                           { Tml.Fst $2 }  
| SND term                                                           { Tml.Snd $2 }  
| INL LPAREN tp RPAREN term                                          { Tml.Inl ($5, $3) }  
| INR LPAREN tp RPAREN term                                          { Tml.Inr ($5, $3) }  
| CASE term OF INL VAR DOUBLEARROW term OR INR VAR DOUBLEARROW term  { Tml.Case ($2, $5, $7, $10, $12) }  
| FIX VAR COLON tp DOUBLEARROW term                                  { Tml.Fix ($2, $4, $6) }
| IF term THEN term ELSE term                                        { Tml.Ifthenelse ($2, $4, $6) }
| PLUS                                                               { Tml.Plus }
| MINUS                                                              { Tml.Minus }
| EQ                                                                 { Tml.Eq }
| LET VAR COLON tp EQ term IN term                                   { Tml.App (Tml.Lam ($2, $4, $8), $6) }
| LET REC VAR COLON tp EQ term IN term                               { Tml.App (Tml.Lam ($3, $5, $9), Tml.Fix ($3, $5, $7)) }
;
