exception NotImplemented
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree
val lrevrev : 'a list list -> 'a list list
val lfoldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val fact : int -> int
val fib : int -> int
val alterSum : int list -> int
val ltabulate : int -> (int -> 'a) -> 'a list
val lfilter : ('a -> bool) -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val inorder : 'a tree -> 'a list
val postorder : 'a tree -> 'a list
val preorder : 'a tree -> 'a list
val quicksort : 'a list -> 'a list
val mergesort : 'a list -> 'a list
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
module Heap : HEAP
module DictList :
  sig
    type key = string
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict
  end
module DictFun :
  sig
    type key = string
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict
  end
