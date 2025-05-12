(* set consists of 'a type elements *)
(* type 'a set *)
type 'a set = 'a list

(* the empty set *)
(* val empty : 'a set *)
let empty = []

(* singleton e returns a set that consists of the element e *)
(* val singleton : 'a -> 'a set *)
let singleton e = [e]

(* mem e s returns true if s contains e 
   mem e s returns false otherwise *)
(* val mem : 'a -> 'a set -> bool *)
let mem e s = List.mem e s

(* add e s returns {e} U s *)
(* val add : 'a -> 'a set -> 'a set *)
let add e s = if mem e s then s else e::s

(* union s1 s2 returns s1 U s2 *)
(* val union : 'a set -> 'a set -> 'a set *)
let union s s' = List.fold_left (fun acc e -> add e acc) s s'

(* diff s1 s2 returns s1 / s2 *)
(* val diff : 'a set -> 'a set -> 'a set *)
let diff s s' = List.filter (fun e -> mem e s' = false) s

(* fold f a s applies the fold operation to s with the function f and the initial result a. the order that the elements in s are processed are random *)
(* val fold : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a *)
let fold f i s = List.fold_left (fun acc e -> f acc e) i s

(* collapse sl returns {x | x belongs to s, s belongs to sl}. *)
(* val collapse : ('a set) set -> 'a set *)
let collapse s = List.fold_left (fun acc e -> union e acc) empty s

(* size s returns the number of elements in s. *)
(* val size : 'a set -> int *)
let size s = List.length s

(* val app : ('a -> unit) -> 'a set -> unit *)
let app f s = List.iter f s
