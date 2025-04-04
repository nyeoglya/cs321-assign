exception Not_implemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec sum n = 
        if n>0 then n + sum (n-1)
        else 0

let rec power x n =
        if n>0 then x * power x (n-1)
        else 1

let rec gcd m n =
        if m=0 then n
        else if n=0 then m
        else if m>n then gcd n (m-n)
        else if m<n then gcd m (n-m)
        else m

let rec combi n k =
        if n>1 && k>1 then combi (n-1) (k-1) + combi (n-1) (k)
        else if n=k || k=0 then 1
        else if k=1 then n
        else 0


let rec sum_tree t = match t with
        | Leaf x -> x
        | Node (left, x, right) -> sum_tree left + x + sum_tree right

let rec depth t = match t with
        | Leaf _ -> 0
        | Node (left, x, right) ->
                let ld = depth(left) in
                let rd = depth(right) in
                        if ld>rd then ld+1
                        else rd+1

let rec bin_search t x = match t with
        | Leaf y -> x=y
        | Node (left, y, right) ->
                if y<x then bin_search right x
                else if y>x then bin_search left x
                else true

let rec postorder t = match t with
        | Leaf y -> [y]
        | Node (left, y, right) -> (postorder left) @ (postorder right) @ [y]


let rec max l = match l with
        | [] -> 0
        | x :: l2 -> 
                let m = max(l2) in
                if x > m then x
                else m

let rec list_add l1 l2 = match l1 with
  | [] -> l2
  | x :: l -> match l2 with
    | [] -> l1
    | y :: l' -> (x+y) :: list_add l l'

let rec insert m l = match l with
  | [] -> [m]
  | x :: l' ->
    if x>m then m :: l
    else x :: insert m l'
        
let rec insort l = match l with
  | [] -> []
  | x :: l' -> insert x (insort l')


let rec compose f g = fun x -> g (f x)

let rec curry f x y = f (x,y)

let rec uncurry f arg = match arg with
  | (x, y) -> f x y

let rec multifun f n = fun x ->
        if n>1 then (multifun f (n-1)) (f x)
        else f x


let rec ltake l n = if n>0 then
                match l with
                        | [] -> []
                        | x :: l' -> x :: ltake l' (n-1)
        else []

let rec lall f l = match l with
        | [] -> true
        | x :: l' -> if f x then lall f l' else false

let rec lmap f l = match l with
        | [] -> []
        | x :: l' -> f x :: lmap f l'

let rec lrev l = match l with
        | [] -> []
        | x :: l' -> lrev l' @ [x]

let rec lflat l = match l with
        | [] -> []
        | x :: l' -> x @ lflat l'


let rec lzip l1 l2 = match l1 with
        | [] -> []
        | x :: l1' -> match l2 with
                | [] -> []
                | y :: l2' -> (x,y) :: lzip l1' l2'

let rec split l = match l with
        | [] -> ([],[])
        | [x] -> ([x],[])
        | x :: y :: l' -> let (la,lb) = split l' in
                (x :: la, y :: lb)

let rec cartprod l1 l2 = match l1 with
        | [] -> []
        | x :: l1' -> match l2 with
                | [] -> []
                | y :: l2' -> (x,y) :: cartprod [x] l2'
                        @ cartprod l1' l2

let rec powerset l = match l with
        | [] -> [[]]
        | x :: l' -> lmap (fun y -> x :: y) (powerset l') @ powerset l'

