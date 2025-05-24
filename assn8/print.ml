(*
val exp2string : Fjava.exp -> string
val method2string : Fjava.methodDecl -> string
val constructor2string : Fjava.constructorDecl -> string
val classlist2string : Fjava.classDecl -> string
val program2string : Fjava.program -> string
*)

open Fjava

let f2fl f l r d =
  let rec fl l =
    match l with
        [] -> ""
      | h :: [] -> f h
      | h :: tl -> (f h) ^ d ^ (fl tl)
  in
  fun ll -> l ^ (fl ll) ^ r

let rec exp2string exp =
  match exp with
      Var x -> x
    | Field (e, f) -> "(" ^ (exp2string e) ^ ")." ^ f
    | Method (e, m, el) -> "(" ^ (exp2string e) ^ ")." ^ m ^ (f2fl exp2string "(" ")" ", " el)
    | New (c, el) -> "new " ^ c ^ (f2fl exp2string "(" ")" ", " el)
    | Cast (c, e) -> "(" ^ c ^ ")" ^ (exp2string e)

let method2string (c, m, cxl, e) = c ^ " " ^ m ^
    (f2fl (fun (c', x') -> c'^" "^x') " (" ") " ", " cxl) ^
    "{ return "^(exp2string e)^"; }\n"

let constructor2string (c, cfl, fl1, ffl2) = c ^
    (f2fl (fun (c', f') -> c' ^ " " ^ f') " (" ") " ", " cfl) ^
    "{ super" ^
    (f2fl (fun f' -> f') "(" "); " ", " fl1) ^
    (f2fl (fun (f1', f2') -> "this."^f1'^" = "^f2') "" "; " "; " ffl2) ^
    "}\n"

let classlist2string (c1, c2, cfl, k, ml) = "class " ^ c1 ^ " extends " ^ c2 ^ " {\n" ^
    (f2fl (fun (c', f') -> c' ^ " " ^ f') "" ";\n" ";\n" cfl) ^
    (constructor2string k) ^
    (f2fl method2string "" "" "" ml) ^
    "}\n"

let program2string (clsl, e) = (f2fl classlist2string "" "\n" "\n" clsl) ^ (exp2string e) ^ "\n"
