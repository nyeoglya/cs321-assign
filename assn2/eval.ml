(*
  eval.ml: ocaml file for testing hw1.ml
*)
open Hw2

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

let _ =
  let test_results = [
    (lrevrev [[1; 2; 3]; [4; 5; 6]; [7]])=[[7]; [6; 5; 4]; [3; 2; 1]];
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
    (union [1; 2; 3] [2; 4; 6])=[6;4;1;2;3];
    (inorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[1; 3; 2; 7; 4];
    (postorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[1; 2; 3; 4; 7];
    (preorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)))=[7; 3; 1; 2; 4];
    (quicksort [3; 7; 5; 1; 2])=[1;2;3;5;7];
    (* (mergesort [3; 7; 5; 1; 2])=[1;2;3;5;7] *)
  ] in
  eval test_results 21

