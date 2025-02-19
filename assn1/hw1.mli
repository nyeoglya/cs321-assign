exception Not_implemented

type 'a tree= Leaf of 'a | Node of 'a tree * 'a * 'a tree

val sum : int -> int
val power : int -> int -> int
val gcd : int -> int -> int
val combi : int -> int -> int

val sum_tree : int tree -> int
val depth : 'a tree -> int
val bin_search : int tree -> int -> bool
val postorder : 'a tree -> 'a list

val max : int list -> int
val list_add : int list -> int list -> int list
val insert : int -> int list -> int list
val insort : int list -> int list

val compose : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
val multifun : ('a -> 'a) -> int -> ('a -> 'a)

val ltake : 'a list -> int -> 'a list
val lall : ('a -> bool) -> 'a list -> bool
val lmap : ('a -> 'b) -> 'a list -> 'b list
val lrev : 'a list -> 'a list
val lflat : 'a list list -> 'a list
val lzip : 'a list -> 'b list -> ('a * 'b) list
val split : 'a list -> 'a list * 'a list
val cartprod : 'a list -> 'b list -> ('a * 'b) list
val powerset : 'a list -> 'a list list

