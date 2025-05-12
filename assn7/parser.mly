%{
%}

%token <string> VAR
%token <int> NUM
%token <bool> TRUE FALSE
%token LPAREN RPAREN INT BOOL UNIT EUNIT PLUS MINUS PROD EQ NEQ ARROW DOUBLEARROW COLON UNDERSCORE FN LET IN END OF VAL REC DATATYPE COMMA OR ENDDEC ENDEXP

%left PLUS MINUS        /* lowest precedence */
%left PROD              /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start parse
%type <Ast.program> parse
%type <Ast.exp> term
%type <Ast.ty> ty
%type <Ast.pat> pat
%type <Ast.conbinding> conbinding
%type <Ast.dec> dec
%type <Ast.mrule> mrule
%%

ty:
| LPAREN ty RPAREN          { $2 }
| INT                       { Ast.T_INT }
| BOOL                      { Ast.T_BOOL }
| UNIT                      { Ast.T_UNIT }
| VAR                       { Ast.T_CON $1 }
| ty ARROW ty               { Ast.T_FUN ($1, $3) }
| ty PROD ty                { Ast.T_PAIR ($1, $3) }

pat:
| LPAREN pat RPAREN         { $2 }
| UNDERSCORE                { Ast.P_WILD }
| NUM                       { Ast.P_INT $1 }
| TRUE                      { Ast.P_BOOL $1 }
| FALSE                     { Ast.P_BOOL $1 }
| EUNIT                     { Ast.P_UNIT }
| VAR                       { Ast.P_VID $1 }
| VAR pat                   { Ast.P_VIDP ($1, $2) }
| pat COMMA pat             { Ast.P_PAIR ($1, $3) }
| pat COLON ty              { Ast.P_TPAT ($1, $3) }

conbinding:
| LPAREN conbinding RPAREN  { $2 }
| VAR                       { Ast.CB_VID $1 }
| VAR OF ty                 { Ast.CB_TVID ($1, $3) }

conbind:
| conbinding                { [$1] }
| conbinding OR conbind     { [$1] @ $3 }

mrule:
| pat DOUBLEARROW term      { Ast.M_RULE ($1, $3) }

mlist:
| mrule                     { [$1] }
| mrule OR mlist            { [$1] @ $3 }

dec: 
| LPAREN dec RPAREN         { $2 }
| VAL pat EQ term           { Ast.D_VAL ($2, $4) }
| VAL REC pat EQ term       { Ast.D_REC ($3, $5) }
| DATATYPE VAR EQ conbind   { Ast.D_DTYPE ($2, $4) }

declist:
| dec                            { [$1] }
| declist ENDDEC dec             { $1 @ [$3] }
| declist ENDDEC dec ENDEXP      { $1 }

parse:
| term ENDEXP                    { ([], $1) }
| declist ENDDEC term ENDEXP     { ($1, $3) }

term:
| appterm                              { $1 }
| term PLUS term                       { Ast.E_APP (Ast.E_PLUS, Ast.E_PAIR ($1, $3)) }
| term MINUS term                      { Ast.E_APP (Ast.E_MINUS, Ast.E_PAIR ($1, $3)) }
| term PROD term                       { Ast.E_APP (Ast.E_MULT, Ast.E_PAIR ($1, $3)) }
| term EQ term                         { Ast.E_APP (Ast.E_EQ, Ast.E_PAIR ($1, $3)) }
| term NEQ term                        { Ast.E_APP (Ast.E_NEQ, Ast.E_PAIR ($1, $3)) }
| LET dec IN term END                  { Ast.E_LET ($2, $4) }
| LPAREN term COLON ty RPAREN          { Ast.E_TEXP ($2, $4) }
| FN mlist                             { Ast.E_FUN $2 }

appterm:
| aterm                                { $1 }
| appterm aterm                        { Ast.E_APP ($1, $2) }

aterm:
| LPAREN term RPAREN                   { $2 }
| LPAREN term COMMA term RPAREN        { Ast.E_PAIR ($2, $4) }                           
| NUM                                  { Ast.E_INT $1 } 
| TRUE                                 { Ast.E_BOOL $1 }                           
| FALSE                                { Ast.E_BOOL $1 } 
| EUNIT                                { Ast.E_UNIT } 
| VAR                                  { Ast.E_VID $1 }
