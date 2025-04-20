open Tml
exception NotImplemented
exception Stuck
exception NotConvertible

type stoval = Computed of value | Delayed of exp * env
  (* Define your own datatypes *)
  and frame = FInd of Heap.loc
    | FApp of exp * env
    | FFst
    | FSnd
    | FCase of exp * exp * env
    | FIf of exp * exp * env
    | FPlus
    | FPlusP of exp * env
    | FPlusPP of value
    | FMinus
    | FMinusP of exp * env
    | FMinusPP of value
    | FEq
    | FEqP of exp * env
    | FEqPP of value
  and stack = Hole_SK | Frame_SK of stack * frame
  and state = Anal_ST of (stoval Heap.heap) * stack * exp * env | Return_ST of (stoval Heap.heap) * stack * value
  and value = Vlam of exp * env
    | Vpair of exp * exp * env
    | Vinl of exp * env
    | Vinr of exp * env
    | Vnum of int
    | Vunit
    | Vtrue
    | Vfalse
    | Vplus
    | Vminus
    | Veq
  and env = Heap.loc list

(* Define your own empty environment *)
let emptyEnv = []

(* Implement the function value2exp : value -> Tml.exp
 * Warning : If you give wrong implementation of this function,
 *           you wiil receive no credit for the entire third part!  *)
let value2exp v = match v with
  | Vnum i -> Num i
  | Vunit -> Eunit
  | Vtrue -> True
  | Vfalse -> False
  | _ -> raise NotConvertible

(* Problem 1.
 * texp2exp : Tml.texp -> Tml.exp *)
let del l x = List.filter (fun y -> y <> x) l

let loc l x =
  let rec laux l x n = match l with
    | [] -> raise Stuck
    | x' :: l' ->
      if x=x' then n
      else laux l' x (n+1)
  in laux l x 0

let rec contain l x = match l with
  | [] -> false
  | x' :: l' ->
      if x=x' then true
      else contain l' x

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | x :: l' ->
      if contain l2 x then union l' l2
      else x :: (union l' l2)

let free e =
  let rec free_aux e l = match e with
    | Tvar v ->
        if (contain l v) then v :: (del l v)
        else v :: l
    | Tlam (v, t, e') -> del (free_aux e' l) v
    | Tapp (e1, e2) -> union (free_aux e1 l) (free_aux e2 [])
    | Tpair (e1, e2) -> union (free_aux e1 l) (free_aux e2 [])
    | Tfst e' -> free_aux e' l
    | Tsnd e' -> free_aux e' l
    | Tinl (e', t) -> free_aux e' l
    | Tinr (e', t) -> free_aux e' l
    | Tcase (e', x1, e1, x2, e2) -> union (union (free_aux e' l) (del (free_aux e1 []) x1)) (del (free_aux e2 []) x2)
    | Tfix (v, t, e') -> del (free_aux e' l) v
    | Tifthenelse (e', e1, e2) -> union (union (free_aux e' l) (free_aux e1 [])) (free_aux e2 [])
    | _ -> l
  in List.rev (free_aux e [])

let texp2exp e =
  let rec taux e l = match e with
    | Tvar v -> Ind (loc l v)
    | Tlam (v, t, e') -> Lam (taux e' (v :: l))
    | Tapp (e1, e2) -> App (taux e1 l, taux e2 l)
    | Tpair (e1, e2) -> Pair (taux e1 l, taux e2 l)
    | Tfst e' -> Fst (taux e' l)
    | Tsnd e' -> Snd (taux e' l)
    | Tinl (e', t) -> Inl (taux e' l)
    | Tinr (e', t) -> Inr (taux e' l)
    | Tcase (e', x1, e1, x2, e2) -> Case (taux e' l, taux e1 (x1 :: l), taux e2 (x2 :: l))
    | Tfix (v, t, e') -> Fix (taux e' (v :: l))
    | Tifthenelse (e', e1, e2) -> Ifthenelse (taux e' l, taux e1 l, taux e2 l)
    | Tnum i -> Num i
    | Teunit -> Eunit
    | Ttrue -> True
    | Tfalse -> False
    | Tplus -> Plus
    | Tminus -> Minus
    | Teq -> Eq
  in taux e (free e)

(* Problem 2. step1 : Tml.exp -> Tml.exp *)
let rec is_value e = match e with
  | Ind v -> false 
  | Lam e' -> true
  | App (e1, e2) -> false
  | Pair (e1, e2) -> (is_value e1) && (is_value e2)
  | Fst e' -> false
  | Snd e' -> false
  | Inl e' -> is_value e'
  | Inr e' -> is_value e'
  | Case (e', e1, e2) -> false
  | Fix e' -> false
  | Ifthenelse (e', e1, e2) -> false
  | Num i -> true
  | Eunit -> true
  | True -> true
  | False -> true
  | Plus -> true
  | Minus -> true
  | Eq -> true

let rec shift e n i = match e with
  | Ind m ->
      if m>=i then Ind (m+n)
      else Ind m
  | Lam n' -> Lam (shift n' n (i+1))
  | App (n1, n2) -> App (shift n1 n i, shift n2 n i)
  | Pair (n1, n2) -> Pair (shift n1 n i, shift n2 n i)
  | Fst n' -> Fst (shift n' n i)
  | Snd n' -> Snd (shift n' n i)
  | Inl n' -> Inl (shift n' n i)
  | Inr n' -> Inr (shift n' n i)
  | Case (n', n1, n2) -> Case (shift n' n i, shift n1 n (i+1), shift n2 n (i+1))
  | Fix n' -> Fix (shift n' n (i+1))
  | Ifthenelse (n', n1, n2) -> Ifthenelse (shift n' n i, shift n1 n i, shift n2 n i)
  | x -> x

let rec sub m n nn =
  let rec sub_aux m n nn = match m with
    | Ind m' ->
        if m'<n then Ind m'
        else if m'>n then Ind (m'-1)
        else shift nn n 0
    | Lam m' -> Lam (sub_aux m' (n+1) nn)
    | App (m1, m2) -> App (sub_aux m1 n nn, sub_aux m2 n nn)
    | Pair (m1, m2) -> Pair (sub_aux m1 n nn, sub_aux m2 n nn)
    | Fst m' -> Fst (sub_aux m' n nn)
    | Snd m' -> Snd (sub_aux m' n nn)
    | Inl m' -> Inl (sub_aux m' n nn)
    | Inr m' -> Inr (sub_aux m' n nn)
    | Case (m', m1, m2) -> Case (sub_aux m' n nn, sub_aux m1 (n+1) nn, sub_aux m2 (n+1) nn)
    | Fix m' -> Fix (sub_aux m' (n+1) nn)
    | Ifthenelse (m', m1, m2) -> Ifthenelse (sub_aux m' n nn, sub_aux m1 n nn, sub_aux m2 n nn)
    | x -> x
  in sub_aux m n nn

let rec step1 e = match e with
  | App (e1, e2) ->
    if is_value e1 then (
      match e1 with
        | Lam e' ->
          if is_value e2 then sub e' 0 e2
          else App (e1, step1 e2)
        | Plus ->
          if is_value e2 then (
            match e2 with
              | Pair (Num i1, Num i2) -> Num (i1+i2)
              | _ -> raise Stuck
          )
          else App (Plus, step1 e2)
        | Minus -> 
          if is_value e2 then (
            match e2 with
              | Pair (Num i1, Num i2) -> Num (i1-i2)
              | _ -> raise Stuck
          )
          else App (Minus, step1 e2)
        | Eq -> 
          if is_value e2 then (
            match e2 with
              | Pair (Num i1, Num i2) ->
                if i1=i2 then True
                else False
              | _ -> raise Stuck
          )
          else App (Eq, step1 e2)
        | _ -> raise Stuck
    )
    else App (step1 e1, e2)
  | Pair (e1, e2) ->
      if (is_value e1) then Pair (e1, step1 e2)
      else Pair (step1 e1, e2)
  | Fst e' ->
      if is_value e' then (
        match e' with
          | Pair (e1, e2) -> e1
          | _ -> raise Stuck
      )
      else Fst (step1 e')
  | Snd e' ->
      if is_value e' then (
        match e' with
          | Pair (e1, e2) -> e2
          | _ -> raise Stuck
      )
      else Snd (step1 e')
  | Inl e' -> Inl (step1 e')
  | Inr e' -> Inr (step1 e')
  | Case (e', e1, e2) ->
    if is_value e' then (
      match e' with
        | Inl ee -> sub e1 0 ee
        | Inr ee -> sub e2 0 ee
        | _ -> raise Stuck
    )
    else Case (step1 e', e1, e2)
  | Fix e' -> sub e' 0 (Fix e')
  | Ifthenelse (e', e1, e2) ->
    if is_value e' then (
      match e' with
        | True -> e1
        | False -> e2
        | _ -> raise Stuck
    )
    else Ifthenelse (step1 e', e1, e2)
  | _ -> raise Stuck

(* Problem 3.
 * step2 : state -> state *)

let find en n =
  let rec faux l n = match l with
    | [] -> raise Stuck
    | x :: l' -> 
        if n=0 then x
        else faux l' (n-1)
  in faux en n

let step2 s = match s with
  | Anal_ST (h, st, e, en) -> (
      match e with
        | Ind n ->
            let l = find en n in
            let sv = Heap.deref h l in (
              match sv with
                | Computed v -> Return_ST (h, st, v)
                | Delayed (e, en') -> Anal_ST (h, Frame_SK (st, FInd l), e, en')
            )
        | Lam m' -> Return_ST (h, st, Vlam (e, en))
        | App (m1, m2) -> Anal_ST (h, Frame_SK (st, FApp (m2, en)), m1, en)
        | Pair (m1, m2) -> Return_ST (h, st, Vpair (m1, m2, en))
        | Fst m' -> Anal_ST (h, Frame_SK (st, FFst), m', en)
        | Snd m' -> Anal_ST (h, Frame_SK (st, FSnd), m', en)
        | Inl m' -> Return_ST (h, st, Vinl (m', en))
        | Inr m' -> Return_ST (h, st, Vinr (m', en))
        | Case (m', m1, m2) -> Anal_ST (h, Frame_SK (st, FCase (m1, m2, en)), m', en)
        | Fix m' -> let hal = Heap.allocate h (Delayed (e, en)) in (
            match hal with
              | (h', l') -> Anal_ST (h', st, m', l' :: en)
        )
        | Ifthenelse (m', m1, m2) -> Anal_ST (h, Frame_SK (st, FIf (m1, m2, en)), m', en)
        | Eunit -> Return_ST (h, st, Vunit)
        | True -> Return_ST (h, st, Vtrue)
        | False -> Return_ST (h, st, Vfalse)
        | Num i -> Return_ST (h, st, Vnum i)
        | Plus -> Return_ST (h, st, Vplus)
        | Minus -> Return_ST (h, st, Vminus)
        | Eq -> Return_ST (h, st, Veq)
  )
  | Return_ST (h, st, v) -> (
      match st with
        | Frame_SK (st', f) -> (
            match f with
              | FInd l -> let hup = Heap.update h l (Computed v) in
                Return_ST (hup, st', v)
              | FApp (e, en) -> (
                  match v with
                    | Vlam (Lam e', en') -> let hal = Heap.allocate h (Delayed (e, en)) in (
                        match hal with
                          | (h', l') -> Anal_ST (h', st', e', l' :: en')
                    )
                    | Vplus -> Anal_ST (h, Frame_SK (st', FPlus), e, en)
                    | Vminus -> Anal_ST (h, Frame_SK (st', FMinus), e, en)
                    | Veq -> Anal_ST (h, Frame_SK (st', FEq), e, en)
                    | _ -> raise Stuck
              )
              | FFst -> (
                  match v with
                    | Vpair (e1, e2, en) -> Anal_ST (h, st', e1, en)
                    | _ -> raise Stuck
              )
              | FSnd -> (
                  match v with
                    | Vpair (e1, e2, en) -> Anal_ST (h, st', e2, en)
                    | _ -> raise Stuck
              )
              | FCase (e1, e2, en) -> (
                  match v with
                    | Vinl (e', en') -> let hal = Heap.allocate h (Delayed (e', en')) in (
                        match hal with
                          | (h', l') -> Anal_ST (h', st', e1, l' :: en)
                    )
                    | Vinr (e', en') -> let hal = Heap.allocate h (Delayed (e', en')) in (
                        match hal with
                          | (h', l') -> Anal_ST (h', st', e2, l' :: en)
                    )
                    | _ -> raise Stuck
              )
              | FIf (e1, e2, en) -> (
                  match v with
                    | Vtrue -> Anal_ST (h, st', e1, en)
                    | Vfalse -> Anal_ST (h, st', e2, en)
                    | _ -> raise Stuck
              )
              | FPlus -> (
                  match v with
                    | Vpair (e1, e2, en) -> Anal_ST (h, Frame_SK (st', FPlusP (e2, en)), e1, en)
                    | _ -> raise Stuck
              )
              | FPlusP (e, en) -> Anal_ST (h, Frame_SK (st', FPlusPP v), e, en)
              | FPlusPP v' -> (
                  match (v, v') with
                    | (Vnum i1, Vnum i2) -> Return_ST (h, st', Vnum (i1+i2))
                    | _ -> raise Stuck
              )
              | FMinus -> (
                  match v with
                    | Vpair (e1, e2, en) -> Anal_ST (h, Frame_SK (st', FMinusP (e2, en)), e1, en)
                    | _ -> raise Stuck
              )
              | FMinusP (e, en) -> Anal_ST (h, Frame_SK (st', FMinusPP v), e, en)
              | FMinusPP v' -> (
                  match (v', v) with
                    | (Vnum i1, Vnum i2) -> Return_ST (h, st', Vnum (i1-i2))
                    | _ -> raise Stuck
              )
              | FEq -> (
                  match v with
                    | Vpair (e1, e2, en) -> Anal_ST (h, Frame_SK (st', FEqP (e2, en)), e1, en)
                    | _ -> raise Stuck
              )
              | FEqP (e, en) -> Anal_ST (h, Frame_SK (st', FEqPP v), e, en)
              | FEqPP v' -> (
                  match (v, v') with
                    | (Vnum i1, Vnum i2) ->
                        if i1=i2 then Return_ST (h, st', Vtrue)
                        else Return_ST (h, st', Vfalse)
                    | _ -> raise Stuck
              )
        )
        | _ -> raise Stuck
  )

(* exp2string : Tml.exp -> string *)
let rec exp2string exp = match exp with
  | Ind x -> string_of_int x
  | Lam e -> "(lam. " ^ (exp2string e) ^ ")"
  | App (e1, e2) -> "(" ^ (exp2string e1) ^ " " ^ (exp2string e2) ^ ")"
  | Pair (e1, e2) -> "(" ^ (exp2string e1) ^ "," ^ (exp2string e2) ^ ")"
  | Fst e -> "(fst " ^ (exp2string e) ^ ")"
  | Snd e -> "(snd " ^ (exp2string e) ^ ")"
  | Eunit -> "()"
  | Inl e -> "(inl " ^ (exp2string e) ^ ")"
  | Inr e -> "(inr " ^ (exp2string e) ^ ")"
  | Case (e, e1, e2) -> "(case " ^ (exp2string e) ^" of " ^ (exp2string e1) ^ " | " ^ (exp2string e2) ^ ")"
  | Fix e -> "(fix. "  ^ (exp2string e) ^ ")"
  | Ifthenelse (e, e1, e2) -> "(if " ^ (exp2string e) ^ " then " ^ (exp2string e1) ^ " else " ^ (exp2string e2) ^ ")"
  | True -> "true"
  | False -> "false"
  | Num n -> "<" ^ (string_of_int n) ^ ">"
  | Plus -> "+"
  | Minus -> "-"
  | Eq -> "="

(* state2string : state -> string
 * you may modify this function for debugging your code *)
let state2string st = match st with
    Anal_ST(_,_,exp,_) -> "Analysis : ???"
  | Return_ST(_,_,_) -> "Return : ??? 
  "
(* ------------------------------------------------------------- *)
let stepOpt1 e = try Some (step1 e) with Stuck -> None
let stepOpt2 st = try Some (step2 st) with Stuck -> None

let rec multiStep1 e = try multiStep1 (step1 e) with Stuck -> e
let rec multiStep2 st = try multiStep2 (step2 st) with Stuck -> st

let stepStream1 e =
  let rec steps e =
    match (stepOpt1 e) with
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in
  Stream.icons e (steps e)

let stepStream2 st =
  let rec steps st =
    match (stepOpt2 st) with
      None -> Stream.from (fun _ -> None)
    | Some st' -> Stream.icons st' (steps st')
  in
  Stream.icons st (steps st)
