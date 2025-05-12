(* val read_file : string -> Ast.program *)
let read_file name =
  let lines = ref "" in
  let channel = open_in name in
  let _ = 
    try
      while true; do
        lines := !lines ^ " " ^ (input_line channel);
      done
    with _ -> close_in channel
  in
  Parser.parse Lexer.token (Lexing.from_string !lines)

(* val write_file : string -> string -> unit *)
let write_file name s = 
  let oc = open_out name in   
  output_string oc s;   
  close_out oc

(* val write_file : string -> string -> unit *)  
let append_file name s =   
  let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 name in
  output_string out s;
  close_out out
