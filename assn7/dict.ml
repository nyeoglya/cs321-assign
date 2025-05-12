(* dictionary with key type ''k and value type 'v *)
type ('k, 'v) dict = ('k * 'v) list

(* the empty dictionary *)
(* val empty : ('k, 'v) dict *)
let empty = []

(* singleton (k, v) returns a dictionary that consists of the binding of k to v *)
(* val singleton : ('k * 'v) -> ('k, 'v) dict *)
let singleton (k, v) = [(k, v)]

(* lookup k d returns SOME v if d contains { k -> v } 
   lookup k d returns NONE if not *)
(* val lookup : 'k -> ('k, 'v) dict -> 'v option *)
let rec lookup k d = match d with
    [] -> None
  | (k', v')::d' -> if k = k' then Some v' else lookup k d' 

let delete k d = List.filter (fun (k', _) -> k <> k') d

(* insert (k, v) d returns a dictionary containing the same set of bindings as d plus a new binding of k to v. *)
(* val insert : ('k * 'v) -> ('k, 'v) dict -> ('k, 'v) dict *)
let insert (k, v) d = (k, v)::(delete k d)

(* map f d retuns a dictionary containing a binding k to f v for each binding k to v in d. *)
(* val map : ('v -> 'w) -> ('k, 'v) dict -> ('k, 'w) dict *)
let map f d = List.map (fun (k, v) -> (k, f v)) d

(* filter f d returns a dictionary containing a binding k to v that f (k, v) is true in d. *)
(* val filter : ('k * 'v -> bool) -> ('k, 'v) dict -> ('k, 'v) dict *)
let filter f d = List.filter (fun (k, v) -> f (k, v)) d

(* merge d d' combines an old dictionary d with a new one d'.
   if d and d' both have a binding associated with k, the binding in d is overriden by that in d'. *)
(* val merge : ('k, 'v) dict -> ('k, 'v) dict -> ('k, 'v) dict *)
let merge old_dict new_dict = List.fold_left (fun old (k, v) -> insert (k, v) old) old_dict new_dict

(* fold f a d applies the fold operation to d with the function f and
   the initial result a. the order that the bindings in k are processed are random. *)
(* val fold : ('w -> 'k * 'v -> 'w) -> 'w -> ('k, 'v) dict -> 'w *)
let fold f a d = List.fold_left (fun acc (k, v) -> f acc (k, v)) a d

(* dom d returns the set of keys in d. *)
(* val dom : ('k, 'v) dict -> 'k Set_type.set *)
let dom d = List.fold_left (fun acc (k, v) -> Set_type.add k acc) Set_type.empty d

(* range d returns the set of values in d. *)
(* val range : ('k, 'v) dict -> 'v Set_type.set *)
let range d = List.fold_left (fun acc (k, v) -> Set_type.add v acc) Set_type.empty d
