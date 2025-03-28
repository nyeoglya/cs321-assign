open Uml

let rec exp2string lam = match lam with
    Var x -> x
  | Lam (v, e) -> "(lam " ^ v ^ ". " ^ (exp2string e) ^ ")" 
  | App (e1, e2) -> "(" ^ (exp2string e1) ^ " " ^ (exp2string e2) ^ ")" 

let rec parse_input () = 
  try Parser.parse Lexer.token (Lexing.from_channel stdin)
  with Parsing.Parse_error -> print_endline "Syntax error";print_string "Uml>:"; flush_all ();
                              parse_input ()
                                          
let rec read_line () = let _ = print_string "Uml> "; flush_all () in 
                       Stream.icons (parse_input ()) (Stream.slazy read_line)
                                    
let read_file name =
  let exps = ref [] in
  let exp = ref "" in 
  let channel = open_in name in
  let _ = 
    try
      while true; do
        exp := !exp ^ " " ^ (input_line channel);
        if String.contains !exp ';' 
        then (exps := !exps @ [Parser.parse Lexer.token (Lexing.from_string !exp)]; exp := "")
      done
    with _ -> close_in channel
  in
  Stream.of_list !exps
                 
