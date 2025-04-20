%{
%}

%token <string> VAR
%token <int> NUM
%token LPAREN RPAREN EOF LET IN EQ COMMA FN COLON BOOL INT UNIT PLUS PROD ARROW FST SND CASE INL INR EUNIT FIX REC MINUS COMMA DOUBLEARROW OF OR TRUE FALSE IF THEN ELSE

%start parse
%type <Tml.texp> parse
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
| FN VAR COLON tp DOUBLEARROW term                                   { Tml.Tlam ($2, $4, $6) }
;

appterm:
| aterm                     { $1 }
| appterm aterm             { Tml.Tapp ($1, $2) }

aterm:
| LPAREN term RPAREN                                                 { $2 }
| VAR                                                                { Tml.Tvar $1 }
| EUNIT                                                              { Tml.Teunit } 
| TRUE                                                               { Tml.Ttrue }                           
| FALSE                                                              { Tml.Tfalse } 
| NUM                                                                { Tml.Tnum $1 } 
| LPAREN term COMMA term RPAREN                                      { Tml.Tpair ($2, $4) }                          
| FST term                                                           { Tml.Tfst $2 }  
| SND term                                                           { Tml.Tsnd $2 }  
| INL LPAREN tp RPAREN term                                          { Tml.Tinl ($5, $3) }  
| INR LPAREN tp RPAREN term                                          { Tml.Tinr ($5, $3) }  
| CASE term OF INL VAR DOUBLEARROW term OR INR VAR DOUBLEARROW term  { Tml.Tcase ($2, $5, $7, $10, $12) }  
| FIX VAR COLON tp DOUBLEARROW term                                  { Tml.Tfix ($2, $4, $6) }
| IF term THEN term ELSE term                                        { Tml.Tifthenelse ($2, $4, $6) }
| PLUS                                                               { Tml.Tplus }
| MINUS                                                              { Tml.Tminus }
| EQ                                                                 { Tml.Teq }
| LET VAR COLON tp EQ term IN term                                   { Tml.Tapp (Tml.Tlam ($2, $4, $8), $6) }
| LET REC VAR COLON tp EQ term IN term                               { Tml.Tapp (Tml.Tlam ($3, $5, $9), Tml.Tfix ($3, $5, $7)) }
;
