let vid2str = fun s -> s
let tycon2str = fun s -> s

let rec ty2str ty = match ty with 
    Ast.T_INT -> "int"
  | Ast.T_BOOL -> "bool"
  | Ast.T_UNIT -> "unit"
  | Ast.T_CON tc -> tycon2str tc
  | Ast.T_PAIR (ty, ty') -> "(" ^ (ty2str ty) ^ ", " ^ (ty2str ty') ^ ")"
  | Ast.T_FUN (ty, ty') -> "(" ^ (ty2str ty) ^ "->" ^ (ty2str ty') ^ ")"

let rec pat2str pat = match pat with
    Ast.P_WILD -> "_"
  | Ast.P_INT i -> string_of_int i
  | Ast.P_BOOL b -> string_of_bool b
  | Ast.P_UNIT -> "()"
  | Ast.P_VID vid -> vid2str vid
  | Ast.P_VIDP (vid, pat) -> (vid2str vid) ^ " " ^ (pat2str pat)
  | Ast.P_PAIR (pat, pat') -> "(" ^ (pat2str pat) ^ ", " ^ (pat2str pat') ^ ")"
  | Ast.P_TPAT (pat, ty) -> (pat2str pat) ^ " : " ^ (ty2str ty)

let conbinding2str conbinding = match conbinding with
    Ast.CB_VID vid -> vid2str vid
  | Ast.CB_TVID (vid, ty) -> (vid2str vid) ^ " of " ^ (ty2str ty)

let rec exp2str exp = match exp with 
    Ast.E_INT i -> string_of_int i
  | Ast.E_BOOL b -> string_of_bool b
  | Ast.E_UNIT -> "()"
  | Ast.E_PLUS -> "+"
  | Ast.E_MINUS -> "-"
  | Ast.E_MULT -> "*"
  | Ast.E_EQ -> "="
  | Ast.E_NEQ -> "<>"
  | Ast.E_VID vid -> vid2str vid
  | Ast.E_PAIR (exp, exp') -> "(" ^ (exp2str exp) ^ ", " ^ (exp2str exp') ^ ")"
  | Ast.E_LET (dec, exp) -> "let\n" ^ (dec2str dec) ^ "\n" ^ "in\n" ^ (exp2str exp) ^ "\n" ^ "end\n" 
  | Ast.E_APP (exp, exp') ->  "(" ^ (exp2str exp) ^ ") (" ^ (exp2str exp') ^ ")"
  | Ast.E_TEXP (exp, ty) -> (exp2str exp) ^ " : " ^ (ty2str ty)
  | Ast.E_FUN mlist -> "fn " ^ (String.concat " | " (List.map mrule2str mlist))

and dec2str dec = match dec with
    Ast.D_VAL (pat, exp) -> "val " ^ (pat2str pat) ^ " = " ^ (exp2str exp)
  | Ast.D_REC (pat, exp) -> "val rec " ^ (pat2str pat) ^ " = " ^ (exp2str exp)
  | Ast.D_DTYPE (tc, cblist) -> "datatype " ^ (tycon2str tc) ^ " = " ^ (String.concat " | " (List.map conbinding2str cblist))

and mrule2str (Ast.M_RULE (pat, exp)) = (pat2str pat) ^ " => " ^ (exp2str exp)

(* val program2str : Ast.program -> string *)
let program2str (dlist, exp) = (String.concat ";\n" (List.map dec2str dlist)) ^ ";\n" ^ (exp2str exp) ^ "\n"
