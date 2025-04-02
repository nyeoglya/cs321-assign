open Tml

exception TypeError

(***************************************************** 
 * replace unit by your own type for typing contexts *
 *****************************************************)


module DictFun =
  struct
    type key = string
    type 'a dict = key -> 'a option

    let empty () = fun x -> None
    let lookup d k = d k
    let get x = match x with
      | Some v -> v
      | None -> raise TypeError
    let insert d (k, v) = fun x ->
      if x=k then Some v
      else d x
  end

open DictFun

type context = string -> tp option

(*
 * For each function you introduce, 
 * write its type, specification, and invariant. 
 *)

let createEmptyContext (): context = DictFun.empty ()

(* val typing : context -> Tml.exp -> Tml.tp *)
let rec typing cxt e = match e with
  | Var x ->
      let x' = lookup cxt x in
      get x'
  | Lam (v, t, e') ->
      let cxt' = insert cxt (v, t) in
      Fun (t, typing cxt' e')
  | App (e1, e2) -> 
      let ft = typing cxt e1 in
      let bt = typing cxt e2 in
      (match ft with
        | Fun (a, b) ->
            if bt=a then b
            else raise TypeError
        | _ -> raise TypeError
      )
  | Pair (e1, e2) -> Prod (typing cxt e1, typing cxt e2)
  | Fst e' -> let et = typing cxt e' in
    (match et with
      | Prod (a, b) -> a
      | _ -> raise TypeError
    )
  | Snd e' -> let et = typing cxt e' in
    (match et with
      | Prod (a, b) -> b
      | _ -> raise TypeError
    )
  | Inl (e', t) -> Sum (typing cxt e', t)
  | Inr (e', t) -> Sum (t, typing cxt e')
  | Ifthenelse (e', e1, e2) ->
      if (typing cxt e')=Bool then
        let a1 = typing cxt e1 in
        let a2 = typing cxt e2 in
          if a1=a2 then a1
          else raise TypeError
      else raise TypeError
  | Case (e', v1, e1, v2, e2) -> (match (typing cxt e') with
      | Sum (a1, a2) ->
          let c1 = typing (insert cxt (v1, a1)) e1 in
          let c2 = typing (insert cxt (v2, a2)) e2 in
            if c1=c2 then c1
            else raise TypeError
      | _ -> raise TypeError
    )
  | Fix (v, t, e') ->
      let cxt' = insert cxt (v, t) in
      typing cxt' e'
  | Eunit -> Unit
  | True -> Bool
  | False -> Bool
  | Num i -> Int
  | Plus -> Fun (Prod (Int, Int), Int)
  | Minus -> Fun (Prod (Int, Int), Int)
  | Eq -> Fun (Prod (Int, Int), Bool)

(*
type tp =                         (* types *) 
    Bool                                (* bool *)
  | Int                                 (* int *)
  | Fun of tp * tp                      (* tp -> tp *)
  | Prod of tp * tp                     (* tp * tp *)
  | Unit                                (* unit *)
  | Sum of tp * tp                      (* tp + tp *)
*)

let typeOf e = typing (createEmptyContext ()) e 
let typeOpt e = try Some (typeOf e) 
                with TypeError -> None



