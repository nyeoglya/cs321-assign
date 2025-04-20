type var = string 
type index = int
               
type tp =                                  (* types *) 
    Bool                                     (* bool *)
  | Int                                      (* int *)
  | Fun of tp * tp                           (* tp -> tp *)
  | Prod of tp * tp                          (* tp * tp *)
  | Unit                                     (* unit *)
  | Sum of tp * tp                           (* tp + tp *)

type texp =
    Tvar of var                              (* variable *)
  | Tlam of var * tp * texp                  (* lambda abstraction *)
  | Tapp of texp * texp                      (* application *)
  | Tpair of texp * texp                     (* pair (e1, e2) *)
  | Tfst of texp                             (* projection fst *)
  | Tsnd of texp                             (* projection snd *)
  | Teunit                                   (* unit *)
  | Tinl of texp * tp                        (* inleft *)
  | Tinr of texp * tp                        (* inright *)
  | Tcase of texp * var * texp * var * texp  (* case e of inl x1 => e1 | inr x2 => e2 *)  
  | Tfix of var * tp * texp                  (* fixed point construct *)
  | Ttrue                                    (* boolean true *)
  | Tfalse                                   (* boolean false *)
  | Tifthenelse of texp * texp * texp        (* conditional construct *)
  | Tnum of int
  | Tplus                                    (* addition *)
  | Tminus                                   (* subtraction *)
  | Teq                                      (* equality *)

type exp = 
    Ind of index                             (* variable *)
  | Lam of exp                               (* lambda abstraction *)
  | App of exp * exp                         (* application *)
  | Pair of exp * exp                        (* pair (e1, e2) *)
  | Fst of exp                               (* projection fst *)
  | Snd of exp                               (* projection snd *)
  | Eunit                                    (* unit *)
  | Inl of exp                               (* inleft *)
  | Inr of exp                               (* inright *)
  | Case of exp * exp * exp                  (* case e of inl e1 | inr e2 *)  
  | Fix of exp                               (* fixed point construct *)
  | True                                     (* boolean true *)
  | False                                    (* boolean false *)
  | Ifthenelse of exp * exp * exp            (* conditional construct *)
  | Num of int 
  | Plus                                     (* addition *)
  | Minus                                    (* subtraction *)
  | Eq                                       (* equality *)


