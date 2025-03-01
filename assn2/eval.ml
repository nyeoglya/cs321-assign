(*
  eval.ml: ocaml file for testing hw2.ml
*)
open Hw2

module type SET =
  sig
    type 'a set

    val empty : unit -> 'a set
    val create : 'a list -> 'a set
    val add : 'a set -> 'a -> 'a set
    val equal : 'a set -> 'a set -> bool
  end

module Set : SET =
struct
  type 'a set = 'a list

  let empty (): 'a set = []
  let create (l: 'a list): 'a set = l
  let add (s: 'a set) (x: 'a): 'a set =
    if List.mem x s then s else x :: s
  let equal (s1: 'a set) (s2: 'a set): bool =
    List.sort compare s1 = List.sort compare s2
end


let rec eval l n = match l with
  | [] -> let _ = print_string "END" in
    print_newline ()
  | x :: l' ->
      let _ =
        let _ = print_string "Test " in
        let _ = print_int n in
        let _ = if x then print_string ": TRUE"
          else print_string ": FALSE" in
        print_newline ()
      in
        eval l' (n+1)

let hupdate h l x =
  let h' = Heap.update h l x in
  (Heap.dereference h' l)=x

let rec dlcreate l =
  match l with
    | [] -> DictList.empty ()
    | (k, v) :: l' -> DictList.insert (dlcreate l') (k, v)

let dldelete dl x =
  let dl' = DictList.delete dl x in
  (DictList.lookup dl x)=None

let rec dfcreate l =
  match l with
    | [] -> DictFun.empty ()
    | (k, v) :: l' -> DictFun.insert (dfcreate l') (k, v)

let dfdelete df x =
  let df' = DictFun.delete df x in
  (DictFun.lookup df x)=None

let ht = Heap.empty ()
let (ht, hi1) = Heap.allocate ht 3
let (ht, hi2) = Heap.allocate ht 5
let (h, hi3) = Heap.allocate ht (-1)

let dl = dlcreate [("one",1); ("three",3); ("two",2)]
let df = dfcreate [("one",1); ("three",3); ("two",2)]

let _ =
  let test_results = [
    (lrevrev [[1; 2; 3]; [4; 5; 6]; [7]])=[[7]; [6; 5; 4]; [3; 2; 1]];
    (lfoldl (fun (x, acc) -> x + acc) 0 [1; 2; 3; 4; 5])=15;
    (lfoldl (fun (x, acc) -> x ^ "," ^ acc) "end" ["a"; "b"; "c"])="a,b,c,end";
    (lfoldl (fun (x, acc) -> x * acc) 1 [2; 3; 4])=24;
    (fact 0)=1;
    (fact 1)=1;
    (fact 2)=2;
    (fact 5)=120;
    (fib 0)=1;
    (fib 1)=1;
    (fib 2)=2;
    (fib 3)=3;
    (fib 4)=5;
    (alterSum [3; 2; 7; 3])=5;
    (alterSum [1])=1;
    (alterSum [])=0;
    (ltabulate 4 (fun x -> x * x))=[0; 1; 4; 9];
    (lfilter (fun x -> x > 2) [0; 1; 2; 3; 4; 5])=[3; 4; 5];
    Set.equal (Set.create (union [1; 2; 3] [2; 4; 6])) (Set.create [6;4;1;3;2]);
    (inorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[1; 3; 2; 7; 4];
    (postorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[1; 2; 3; 4; 7];
    (preorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[7; 3; 1; 2; 4];
    (quicksort [3; 7; 5; 1; 2])=[1;2;3;5;7];
    (mergesort [3; 7; 5; 1; 2])=[1;2;3;5;7]
  ] in
  eval test_results 44

