(*
 * Call-by-value reduction   
 *)

exception NotImplemented 
exception Stuck

let freshVarCounter = ref 0
                          
(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s. 
 *)
let getFreshVariable s = 
  let _ = freshVarCounter := !freshVarCounter + 1
  in
  s ^ "__" ^ (string_of_int (!freshVarCounter))
               
(*
 * implement a single step with reduction using the call-by-value strategy.
 *)

open Uml

module ListFun =
  struct
    type value = var

    let create v = fun x ->
      if x=v then true 
      else false
    let lookup l v = l v
    let delete l v = fun x ->
      if x=v then false
      else l x
    let concat l1 l2 = fun x ->
      l1 x || l2 x
end

let rec swap (e: exp) (x: var) (y: var) = match e with
  | Var v ->
      if v=x then Var y
      else if v=y then Var x
      else e
  | Lam (v, e') ->
      if v=x then Lam (y, swap e' x y)
      else if v=y then Lam (x, swap e' x y)
      else Lam (v, swap e' x y)
  | App (e1, e2) -> App (swap e1 x y, swap e2 x y)

let rec fv (e: exp) = match e with
  | Var v -> ListFun.create v
  | Lam (v, e') -> ListFun.delete (fv e') v
  | App (e1, e2) -> ListFun.concat (fv e1) (fv e2)

let rec sub (e: exp) (x: var) (e': exp) = match e with
  | Var v ->
    if v=x then e' else e
  | App (e1, e2) -> App (sub e1 x e', sub e2 x e')
  | Lam (v, e1) ->
    if v=x then e
    else
      if (ListFun.lookup (fv e') v)=false then Lam (v, sub e1 x e')
      else let v' = getFreshVariable v in
        Lam (v', sub (swap e1 v v') x e')

let rec stepv (e: exp) = match e with
  | Var _ -> raise Stuck
  | Lam (v, e') -> raise Stuck
  | App (e1, e2) -> match e1 with
    | Var _ -> raise Stuck
    | App (_, _) -> App (stepv e1, e2)
    | Lam (v1, e1') -> match e2 with
        | Var _ -> App (e1, stepv e2)
        | App (_, _) -> App (e1, stepv e2)
        | Lam (_, _) -> sub e1' v1 e2

let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e = 
    match (stepOpt stepf e) with 
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)

