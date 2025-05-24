(*
  type action = Fjava.program -> unit

  val show      : action
  val showType  : action
  val eval      : action -> action
  val step      : action -> action
  val wait      : action -> action
  val loop      : action -> unit
  val loopFile  : string -> action -> unit
*)

type action = Fjava.program -> unit

let typing p =
  match TypingEval.typeOpt p with
      Some t -> " : " ^ t
    | None -> " has no type."

let show p = print_endline (Print.program2string p)
let showProgramType p = print_endline ((Print.program2string p) ^ (typing p))
let showType ((_, e) as p) = print_endline ((Print.exp2string e) ^ (typing p))

let eval action p = action (TypingEval.multiStep p)
let step action p = Stream.iter action (TypingEval.stepStream p)
let wait action p =
  let _ = action p;
          print_string "Press return:"; flush_all ();
          input_line stdin
  in ()

(* let loop action = applyAction action (Input.promptkeybd "Fjava> ") *)
let loopFile filename action =
  let ch = open_in filename in
  let _ = Opal.FJavaParser.applyAction action ch in
  close_in ch
