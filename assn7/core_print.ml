let avid2str av = av
let vid2str (av, _) = avid2str av

let tyname2str = string_of_int
let tyvar2str = string_of_int

let rec ty2str ty = match ty with
    Core.T_INT -> "int"
  | Core.T_BOOL -> "bool"
  | Core.T_UNIT -> "unit"
  | Core.T_NAME tn -> tyname2str tn
  | Core.T_PAIR (ty, ty') -> "(" ^ (ty2str ty) ^ ", " ^ (ty2str ty') ^ ")"
  | Core.T_FUN (ty, ty') -> "(" ^ (ty2str ty) ^ "->" ^ (ty2str ty') ^ ")"
  | Core.T_VAR tv -> "'" ^ (tyvar2str tv)

let rec pat2str pat = match pat with 
    Core.P_WILD -> "_"
  | Core.P_INT i -> string_of_int i
  | Core.P_BOOL b -> string_of_bool b
  | Core.P_UNIT -> "()"
  | Core.P_VID vid -> vid2str vid
  | Core.P_VIDP (vid, patty) -> (vid2str vid) ^ " " ^ (patty2str patty)
  | Core.P_PAIR (patty, patty') -> "(" ^ (patty2str patty) ^ ", " ^ (patty2str patty') ^ ")"
and patty2str (Core.PATTY (pat, ty)) = "(" ^ (pat2str pat) ^ " : " ^ (ty2str ty) ^ ")"

let rec exp2str exp = match exp with
    Core.E_INT i -> string_of_int i
  | Core.E_BOOL b -> string_of_bool b
  | Core.E_UNIT -> "()"
  | Core.E_PLUS -> "+"
  | Core.E_MINUS -> "-"
  | Core.E_MULT -> "*"
  | Core.E_EQ -> "="
  | Core.E_NEQ -> "<>"
  | Core.E_VID vid -> vid2str vid
  | Core.E_PAIR (expty, expty') -> "(" ^ (expty2str expty) ^ ", " ^ (expty2str expty') ^ ")"
  | Core.E_LET (dec, expty) -> "let " ^ (dec2str dec) ^ " in " ^ (expty2str expty) ^ " end" 
  | Core.E_APP (expty, expty') ->  "(" ^ (expty2str expty) ^ ") (" ^ (expty2str expty') ^ ")"
  | Core.E_FUN mlist -> "fn " ^ (String.concat " | " (List.map mrule2str mlist))
and expty2str (Core.EXPTY (exp, ty)) = "(" ^ (exp2str exp) ^ " : " ^ (ty2str ty) ^ ")"

and dec2str dec = match dec with
    Core.D_VAL (patty, expty) -> "val " ^ (patty2str patty) ^ " = " ^ (expty2str expty)
  | Core.D_REC (patty, expty) -> "val rec " ^ (patty2str patty) ^ " = " ^ (expty2str expty)
  | Core.D_DTYPE -> "datatype" 

and mrule2str (Core.M_RULE (patty, expty)) = (patty2str patty) ^ " => " ^ (expty2str expty)

(* val program2str : Core.program -> string *)
let program2str (dl, et) = (List.fold_left (fun acc d -> acc ^ (dec2str d) ^ ";\n") "" dl) ^ "\n" ^ (expty2str et) ^ ";;\n"

