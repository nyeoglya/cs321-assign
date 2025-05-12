exception AstValidateError

(* vpat : Ast.pat -> Ast.pat *)
let vpat pat =
  let rec vpat' s pat = 
    match pat with
      Ast.P_WILD -> s
    | Ast.P_INT n -> s
    | Ast.P_BOOL b -> s
    | Ast.P_UNIT -> s
    | Ast.P_VID v -> if Set_type.mem v s then raise AstValidateError else Set_type.add v s
    | Ast.P_VIDP (_, pat) -> vpat' s pat
    | Ast.P_PAIR (pat1, pat2) -> vpat' (vpat' s pat1) pat2
    | Ast.P_TPAT (pat, _) -> vpat' s pat in
  let _ = vpat' Set_type.empty pat in
  pat
    
(* vconbind : Ast.conbinding list -> Ast.conbinding list *)
let vconbind conbind =
  let rec vconbind' s l = 
    match l with 
      [] -> s
    | (Ast.CB_VID v) :: conbind -> if Set_type.mem v s then raise AstValidateError else vconbind' (Set_type.add v s) conbind
    | (Ast.CB_TVID (v, t)) :: conbind -> if Set_type.mem v s then raise AstValidateError else vconbind' (Set_type.add v s) conbind in
  let _ = vconbind' Set_type.empty conbind in
  conbind

(* vexp : Ast.exp -> Ast.exp *)
let rec vexp e = 
  (match e with
     Ast.E_INT n -> Ast.E_INT n
   | Ast.E_BOOL b -> Ast.E_BOOL b
   | Ast.E_UNIT -> Ast.E_UNIT
   | Ast.E_PLUS -> Ast.E_PLUS
   | Ast.E_MINUS -> Ast.E_MINUS
   | Ast.E_MULT -> Ast.E_MULT
   | Ast.E_EQ -> Ast.E_EQ
   | Ast.E_NEQ -> Ast.E_NEQ
   | Ast.E_VID v -> Ast.E_VID v
   | Ast.E_FUN mlist -> Ast.E_FUN (List.map vmrule mlist)
   | Ast.E_APP (exp1, exp2) -> Ast.E_APP (vexp exp1, vexp exp2)
   | Ast.E_PAIR (exp1, exp2) -> Ast.E_PAIR (vexp exp1, vexp exp2)
   | Ast.E_LET (dec, exp) -> Ast.E_LET (vdec dec, vexp exp)
   | Ast.E_TEXP (exp, ty) -> Ast.E_TEXP (vexp exp, ty)
  )
(* vdec : Ast.dec -> Ast.dec *)
and vdec dec = 
  (match dec with
     Ast.D_VAL (Ast.P_BOOL _, _) -> raise AstValidateError
   | Ast.D_VAL (pat, exp) -> (Ast.D_VAL (vpat pat, vexp exp))
   | Ast.D_REC (Ast.P_VID v, Ast.E_FUN mlist) -> Ast.D_REC (Ast.P_VID v, Ast.E_FUN (List.map vmrule mlist))
   | Ast.D_REC (_, _) -> raise AstValidateError
   | Ast.D_DTYPE (tc, cblist) -> Ast.D_DTYPE (tc, vconbind cblist)
  )
(* vmrule : Ast.mrule -> Ast.mrule *)
and vmrule (Ast.M_RULE (pat, exp)) = Ast.M_RULE (vpat pat, vexp exp)

(* vprogram : Ast.program -> Ast.program *)
let vprogram (dlist, exp) = (List.map vdec dlist, vexp exp)
