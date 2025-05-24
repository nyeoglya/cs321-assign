type typ = string
type exp =
    Var of string
  | Field of exp * string
  | Method of exp * string * exp list
  | New of typ * exp list
  | Cast of typ * exp

(* Return Type, Method Name, Parameters, Method Body *)
type methodDecl = typ * string * (typ * string) list * exp

(* Class Name, Parameters, Arguments of super(), Assignments *)
type constructorDecl = typ * (typ * string) list * string list * (string * string) list

(* Class Name, Super Class, Fields, Constructor, Methods *)
type classDecl = typ * typ * (typ * string) list * constructorDecl * (methodDecl list)

(* Classes, Expression *)
type program = classDecl list * exp
