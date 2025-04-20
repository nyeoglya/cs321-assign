exception NotImplemented
exception Stuck
exception NotConvertible

type env
type value 
type frame

type stoval = Computed of value | Delayed of Tml.exp * env
 and stack = Hole_SK | Frame_SK of stack * frame
 and state =
   Anal_ST of stoval Heap.heap * stack * Tml.exp * env
   | Return_ST of stoval Heap.heap * stack * value

val emptyEnv : env
val value2exp : value -> Tml.exp
 
(* translate TML expressions into expressions with de Bruijn's index *)
val texp2exp : Tml.texp -> Tml.exp

(* one-step reduction, raises Stuck if impossible *)
val step1 : Tml.exp -> Tml.exp
val step2 : state -> state

val exp2string : Tml.exp -> string
val state2string : state -> string

(* ... returns NONE if impossible *)
val stepOpt1 : Tml.exp -> Tml.exp option
val stepOpt2 : state -> state option

(* repeats step as many times as possible *)
val multiStep1 : Tml.exp -> Tml.exp
val multiStep2 : state -> state

(* a stream of all steps *)
val stepStream1 : Tml.exp -> Tml.exp Stream.t
val stepStream2 : state -> state Stream.t
