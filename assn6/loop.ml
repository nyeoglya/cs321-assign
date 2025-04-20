type t_action = Tml.texp -> unit
type action1 = Tml.exp -> unit
(*type action2 = Eval.state -> unit*)

let show1 e = print_endline (Eval.exp2string e)
let show2 st = print_endline (Eval.state2string st)

let eval1 action e = action (Eval.multiStep1 (Eval.texp2exp e))
let eval2 action st = action (Eval.multiStep2 
                                (Eval.Anal_ST(Heap.empty, Eval.Hole_SK, (Eval.texp2exp st), Eval.emptyEnv)))

let wait1 action e =
  let _ = action e; 
          print_string "Press return:"; flush_all ();
          input_line stdin 
  in ()
let wait2 action st =
  let _ = action st; 
          print_string "Press return:"; flush_all ();
          input_line stdin 
  in ()

let step1 action e = Stream.iter action (Eval.stepStream1 (Eval.texp2exp e))
let step2 action st = Stream.iter action (Eval.stepStream2 
                                            (Eval.Anal_ST(Heap.empty, Eval.Hole_SK, (Eval.texp2exp st), Eval.emptyEnv)))

let loop action = Stream.iter action (Inout.read_line ())
let loopFile name action = Stream.iter action (Inout.read_file name)
