(*
  eval.ml: ocaml file for testing hw1.ml
*)
open Hw1

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

let comp_f x = x + 1
let comp_g x = 3*x
let multiply x y = x * y
let multiplyUC (x, y) = x * y

let _ =
  let test_results = [
    (sum 10)=55;
    (sum 9)=45;
    (power 10 0)=1;
    (power 3 5)=243;
    (gcd 0 0)=0;
    (gcd 1 0)=1;
    (gcd 0 1)=1;
    (gcd 4 6)=2;
    (combi 1 0)=1;
    (combi 6 3)=20;
    (combi 3 3)=1;
    (sum_tree (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=17;
    (depth (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=2;
    (bin_search (Node (Node (Leaf 1, 2, Leaf 3), 4, Leaf 7)) 2)=true;
    (bin_search (Node (Node (Leaf 1, 2, Leaf 3), 4, Leaf 7)) 5)=false;
    (postorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[1;2;3;4;7];
    (max [5; 3; 6; 7; 4])=7;
    (list_add [1; 2] [3; 4; 5])=[4; 6; 5];
    (list_add [3; 4; 5] [1;2])=[4; 6; 5];
    (insert 3 [1; 2; 4; 5])=[1; 2; 3; 4; 5];
    (insort [3; 7; 5; 1; 2])=[1; 2; 3; 5; 7];
    ((compose comp_f comp_g) 7)=24;
    ((curry multiplyUC) 3 6)=(multiply 3 6);
    ((uncurry multiply) (2, 7))=(multiplyUC (2, 7));
    ((multifun (fun x -> x + 1) 3) 1)=4;
    ((multifun (fun x -> x * x) 3) 2)=256;
    (ltake [3; 7; 5; 1; 2] 3)=[3; 7; 5];
    (ltake [3; 7; 5; 1; 2] 7)=[3; 7; 5; 1; 2];
    (ltake ["s"; "t"; "r"; "i"; "k"; "e"; "r"; "z"] 5)=["s"; "t"; "r"; "i"; "k"];
    (lall (fun x -> x > 0) [1; 2; 3])=true;
    (lall (fun x -> x > 0) [-1; -2; 3])=false;
    (lmap (fun x -> x + 1) [1; 2; 3])=[2; 3; 4];
    (lrev [1; 2; 3; 4])=[4; 3; 2; 1];
    (lflat [[1; 2]; [3; 4; 5]; [6]])=[1; 2; 3; 4; 5; 6];
    (lzip ["Rooney"; "Park"; "Scholes"; "C.Ronaldo"] [8; 13; 18; 7; 10; 12])=[("Rooney", 8); ("Park", 13); ("Scholes", 18); ("C.Ronaldo", 7)];
    (split [1; 3; 5; 7; 9; 11])=([1; 5; 9], [3; 7; 11]);
    (cartprod [1; 2] [3; 4; 5])=[(1, 3); (1, 4); (1, 5); (2, 3); (2, 4); (2, 5)];
    (powerset [1; 2])=[[1; 2]; [1]; [2]; []]
  ] in
  eval test_results 26

