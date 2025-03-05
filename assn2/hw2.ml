exception NotImplemented
	    
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree
						      
(** Recursive functions **)

let rec lrevrev l =
  let rec lrev sl = match sl with
    | [] -> []
    | sx :: sl' -> (lrev sl') @ [sx]
  in
    match l with
      | [] -> []
      | x :: l' -> (lrevrev l') @ [lrev x]

let rec lfoldl f e l =
  let rec lfoldl_aux f l acc =
    match l with
      | [] -> acc
      | x :: l' -> lfoldl_aux f l' (f (x, acc))
  in lfoldl_aux f l e

(** Tail recursive functions **)

let fact n =
  let rec fact_aux n acc =
    if n>1 then fact_aux (n-1) (acc*n)
    else acc
  in fact_aux n 1

let fib n =
  let rec fib_aux n f1 f2 =
    if n>1 then fib_aux (n-1) f2 (f1+f2)
    else f2
  in fib_aux n 1 1

let alterSum l =
  let rec alterSum_aux l fl acc =
    match l with
      | [] -> acc
      | x :: l' ->
          if fl then alterSum_aux l' false (acc+x)
          else alterSum_aux l' true (acc-x)
  in alterSum_aux l true 0

let ltabulate n f =
  let rec ltabulate_aux n f acc =
    if n>=1 then ltabulate_aux (n-1) f ((f (n-1)) :: acc)
    else acc
  in ltabulate_aux n f []

let lfilter p l =
  let rec lfilter_aux p l acc =
    match l with
      | [] -> acc
      | x :: l' ->
          if p x then lfilter_aux p l' (acc @ [x])
          else lfilter_aux p l' acc
  in lfilter_aux p l []

let rec union s t =
  match t with
    | [] -> s
    | x :: t' ->
        let rec contain x l = (* Try finding ways not using this function *)
          match l with
            | [] -> false
            | y :: l' ->
                if x=y then true
                else contain x l'
        in
          if contain x s then union s t'
          else union (x :: s) t'

(*
let inorder t =
  let rec inorder_aux t acc =
    match t with
      | Leaf (x) -> x :: acc
      | Node (lt, x, rt) -> inorder_aux lt (x :: inorder_aux rt acc)
  in inorder_aux t []

idea2: using lazy evaluation??
*)

let inorder t =
  let rec inorder_aux s acc = (* Try finding ways to implement inner function with types ['a tree -> 'a list -> 'a list] *)
    match s with
      | [] -> acc
      | Leaf (x) :: s' -> inorder_aux s' (acc @ [x])
      | Node (lt, x, rt) :: s' -> inorder_aux (lt :: Leaf (x) :: rt :: s') acc
  in inorder_aux [t] []

let postorder t =
  let rec postorder_aux s acc =
    match s with
      | [] -> acc
      | Leaf (x) :: s' -> postorder_aux s' (acc @ [x])
      | Node (lt, x, rt) :: s' -> postorder_aux (lt :: rt :: Leaf (x) :: s') acc
  in postorder_aux [t] []

let preorder t =
  let rec preorder_aux s acc =
    match s with
      | [] -> acc
      | Leaf (x) :: s' -> preorder_aux s' (acc @ [x])
      | Node (lt, x, rt) :: s' -> preorder_aux (Leaf (x) :: lt :: rt :: s') acc
  in preorder_aux [t] []


(** Sorting in the ascending order **)

let rec quicksort l =
  match l with
    | [] -> []
    | p :: l' ->
        let l1 = lfilter (fun x -> x < p) l' in
        let l2 = lfilter (fun x -> x >= p) l' in (* Make lfilter internal *)
          (quicksort l1) @ [p] @ (quicksort l2)

let rec mergesort l =
  match l with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let rec split l = match l with
          | [] -> ([], [])
          | [x] -> ([x] ,[])
          | x :: y :: l' ->
              let (ll, rl) = split l' in
              (x :: ll, y :: rl)
        in
          let (ls, rs) = split l in
          let lm = mergesort ls in
          let rm = mergesort rs
        in
          let rec merge l1 l2 = match l1 with
            | [] -> l2
            | x :: l1' -> match l2 with
              | [] -> l1
              | y :: l2' ->
                  if x<y then x :: merge l1' l2
                  else y :: merge l1 l2'
        in
          merge lm rm

(** Structures **)

module type HEAP =
  sig
    exception InvalidLocation
    type loc
    type 'a heap
    val empty : unit -> 'a heap
    val allocate : 'a heap -> 'a -> 'a heap * loc
    val dereference : 'a heap -> loc -> 'a
    val update : 'a heap -> loc -> 'a -> 'a heap
  end

module type DICT =
  sig
    type key
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict
  end

module Heap : HEAP =
  struct
    exception InvalidLocation
		
    type loc = int
    type 'a heap = Nil | Con of int * 'a * 'a heap

    let empty () = Nil
    let allocate h v = match h with
      | Nil -> (Con (0, v, Nil), 0)
      | Con (pl, _, _) -> (Con (pl+1, v, h), pl+1)
    let rec dereference h l = match h with
      | Nil -> raise InvalidLocation
      | Con (pl, pv, ph) ->
          if pl>l then dereference ph l
          else if pl=l then pv
          else raise InvalidLocation
    let rec update h l v = match h with
      | Nil -> raise InvalidLocation
      | Con (pl, pv, ph) ->
          if pl>l then Con (pl, pv, (update ph l v))
          else if pl=l then Con (pl, v, ph)
          else raise InvalidLocation
  end

module DictList : DICT with type key = string =
  struct
    type key = string
    type 'a dict = (key * 'a) list
		
    let empty () = []
    let rec lookup (d: 'a dict) (k: key) = match d with
      | [] -> None
      | (x, i) :: d' ->
          if x=k then Some i
          else lookup d' k
    let rec delete d k = match d with
      | [] -> []
      | (x, i) :: d' ->
          if x=k then d'
          else (x, i) :: delete d' k
    let rec insert d (k, v) =
      let dd =
        if (lookup d k)=None then d
        else delete d k
      in (k, v) :: dd
  end

module DictFun : DICT with type key = string =
  struct
    type key = string
    type 'a dict = key -> 'a option
		
    let empty () = fun x -> None
    let lookup d k = d k
    let delete d k = fun x ->
      if x=k then None
      else d k
    let insert d (k, v) = fun x ->
      if x=k then Some v
      else d k
  end
