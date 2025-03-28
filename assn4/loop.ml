type action = Uml.exp -> unit
                           
let show e = print_endline (Inout.exp2string e)
                           
let eval stepf action e = action (Eval.multiStep stepf e)
                           
let wait action e =
  let _ = action e; 
          print_string "Press return:"; flush_all ();
          input_line stdin 
  in ()
       
let step stepf action e = Stream.iter action (Eval.stepStream stepf e)
                                
(* Running the actions on an interactive loop or a file *)

let loop action = Stream.iter action (Inout.read_line ())
                              
let loopFile name action = Stream.iter action (Inout.read_file name)
