%{
%}

%token <string> VAR
%token LPAREN RPAREN EOF LET IN EQ DOT LAM

%start parse
%type <Uml.exp> parse
%%

parse:
| term EOF                    { $1 }
;

term:
| appterm                     { $1 }                          
| LAM VAR DOT term            { Uml.Lam ($2, $4) }
;

appterm:
| aterm                       { $1 }
| appterm aterm               { Uml.App ($1, $2) }

aterm:
| LPAREN term RPAREN          { $2 }
| VAR                         { Uml.Var $1 }
| LET VAR EQ term IN term     { Uml.App (Uml.Lam ($2, $6), $4) }
;



