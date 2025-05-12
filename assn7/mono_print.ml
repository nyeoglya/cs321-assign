let avid2str av = av
let vid2str (av, _) = avid2str av

let tyname2str = string_of_int

let rec ty2str ty = match ty with
    Mono.T_INT -> "int"
  | Mono.T_BOOL -> "bool"
  | Mono.T_UNIT -> "unit"
  | Mono.T_NAME tn -> tyname2str tn
  | Mono.T_PAIR (ty, ty') -> "(" ^ (ty2str ty) ^ ", " ^ (ty2str ty') ^ ")"
  | Mono.T_FUN (ty, ty') -> "(" ^ (ty2str ty) ^ "->" ^ (ty2str ty') ^ ")"

let rec pat2str pat = match pat with 
    Mono.P_WILD -> "_"
  | Mono.P_INT i -> string_of_int i
  | Mono.P_BOOL b -> string_of_bool b
  | Mono.P_UNIT -> "()"
  | Mono.P_VID vid -> vid2str vid
  | Mono.P_VIDP (vid, patty) -> (vid2str vid) ^ " " ^ (patty2str patty)
  | Mono.P_PAIR (patty, patty') -> "(" ^ (patty2str patty) ^ ", " ^ (patty2str patty') ^ ")"
and patty2str (Mono.PATTY (pat, ty)) = "(" ^ (pat2str pat) ^ " : " ^ (ty2str ty) ^ ")"

let rec exp2str exp = match exp with
    Mono.E_INT i -> string_of_int i
  | Mono.E_BOOL b -> string_of_bool b
  | Mono.E_UNIT -> "()"
  | Mono.E_PLUS -> "+"
  | Mono.E_MINUS -> "-"
  | Mono.E_MULT -> "*"
  | Mono.E_EQ -> "="
  | Mono.E_NEQ -> "<>"
  | Mono.E_VID vid -> vid2str vid
  | Mono.E_PAIR (expty, expty') -> "(" ^ (expty2str expty) ^ ", " ^ (expty2str expty') ^ ")"
  | Mono.E_LET (dec, expty) -> "let " ^ (dec2str dec) ^ " in " ^ (expty2str expty) ^ " end" 
  | Mono.E_APP (expty, expty') ->  "(" ^ (expty2str expty) ^ ") (" ^ (expty2str expty') ^ ")"
  | Mono.E_FUN mlist -> "fn " ^ (String.concat " | " (List.map mrule2str mlist))
and expty2str (Mono.EXPTY (exp, ty)) = "(" ^ (exp2str exp) ^ " : " ^ (ty2str ty) ^ ")"

and dec2str dec = match dec with
    Mono.D_VAL (patty, expty) -> "val " ^ (patty2str patty) ^ " = " ^ (expty2str expty)
  | Mono.D_REC (patty, expty) -> "val rec " ^ (patty2str patty) ^ " = " ^ (expty2str expty)
  | Mono.D_DTYPE -> "datatype" 

and mrule2str (Mono.M_RULE (patty, expty)) = (patty2str patty) ^ " => " ^ (expty2str expty)

(* val program2str : Mono.program -> string *)
let program2str (dl, et) = (List.fold_left (fun acc d -> acc ^ (dec2str d) ^ ";\n") "" dl) ^ "\n" ^ (expty2str et) ^ ";;\n"

