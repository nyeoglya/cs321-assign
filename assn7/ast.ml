type vid = string           (* <variable identifier> *)
type tycon = string         (* <type constructor> *)

type ty =
    T_INT                   (* int *)
  | T_BOOL                  (* bool *)
  | T_UNIT                  (* unit *)
  | T_CON of tycon          (* tycon *)
  | T_PAIR of ty * ty       (* (ty * ty) *)
  | T_FUN of ty * ty        (* (ty -> ty) *)

type pat =
    P_WILD                  (* _ *)
  | P_INT of int            (* num *)
  | P_BOOL of bool          (* true | false *)
  | P_UNIT                  (* () *)
  | P_VID of vid            (* vid *)
  | P_VIDP of vid * pat     (* vid pat *)
  | P_PAIR of pat * pat     (* (pat, pat) *)
  | P_TPAT of pat * ty      (* (pat : ty) *)

type conbinding =
    CB_VID of vid           (* vid *)
  | CB_TVID of vid * ty     (* vid of ty *)

type exp =
    E_INT of int            (* num *)
  | E_BOOL of bool          (* true | false *)
  | E_UNIT                  (* () *)
  | E_PLUS                  (* + *)
  | E_MINUS                 (* - *)
  | E_MULT                  (* * *)
  | E_EQ                    (* = *)
  | E_NEQ                   (* <> *)
  | E_VID of vid            (* vid *)
  | E_FUN of mrule list     (* fn match *)
  | E_APP of exp * exp      (* exp exp *)
  | E_PAIR of exp * exp     (* (exp, exp) *)
  | E_LET of dec * exp      (* let dec in exp end *)
  | E_TEXP of exp * ty      (* (exp : ty) *)

 and mrule = 
   M_RULE of pat * exp      (* pat => exp *)

 and dec =
   D_VAL of pat * exp                   (* val pat = exp *)
   | D_REC of pat * exp                 (* rec val pat = exp *)
   | D_DTYPE of tycon * conbinding list (* datatype tycon = conbind *)

type program = dec list * exp           (* program *)
