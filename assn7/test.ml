module MonoSample =
  struct
  (* 1 + 1 *)
  let s1 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_PLUS, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_INT)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 1, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_INT))

  (* 1 - 1 *)
  let s2 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_MINUS, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_INT)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 1, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_INT))

  (* 2 * 3 *)
  let s3 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_MULT, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_INT)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 2, Mono.T_INT), Mono.EXPTY (Mono.E_INT 3, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_INT))

  (* 1 = 1 *)
  let s4 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_EQ, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_BOOL)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 1, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_BOOL))

  (* 1 = 2 *)
  let s5 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_EQ, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_BOOL)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 2, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_BOOL))

  (* 1 <> 1 *)
  let s6 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_NEQ, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_BOOL)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 1, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_BOOL))

  (* 1 <> 2 *)
  let s7 : Mono.program = 
    ([], 
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_NEQ, Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_INT), Mono.T_BOOL)), 
         Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 2, Mono.T_INT)), Mono.T_PAIR (Mono.T_INT, Mono.T_INT))), 
       Mono.T_BOOL))

  (* (1, 2) *)
  let s8 : Mono.program =
    ([],
     Mono.EXPTY (
       Mono.E_PAIR (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), Mono.EXPTY (Mono.E_INT 2, Mono.T_INT)),
       Mono.T_PAIR (Mono.T_INT, Mono.T_INT)))

  (* (fn x => x) 1  *)
  let s9 : Mono.program =
    ([],
     Mono.EXPTY (
       Mono.E_APP (
         Mono.EXPTY (Mono.E_FUN [Mono.M_RULE (Mono.PATTY (Mono.P_VID ("x", Mono.VAR), Mono.T_INT), Mono.EXPTY (Mono.E_VID ("x", Mono.VAR), Mono.T_INT))], Mono.T_FUN (Mono.T_INT, Mono.T_INT)),
         Mono.EXPTY (Mono.E_INT 1, Mono.T_INT)),
       Mono.T_INT))
    
  (* val x = 1; x *)
  let s10 : Mono.program =
    ([Mono.D_VAL (
      Mono.PATTY (Mono.P_VID ("x", Mono.VAR), Mono.T_INT), 
      Mono.EXPTY (Mono.E_INT 1, Mono.T_INT))],
     Mono.EXPTY (Mono.E_VID ("x", Mono.VAR), Mono.T_INT))

  (* poly.tml *)
  let poly_tml : Mono.program =
    ([],
     Mono.EXPTY (
       Mono.E_FUN [
         Mono.M_RULE (
           Mono.PATTY (Mono.P_VID ("f", Mono.VAR), Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT)),
           Mono.EXPTY (
             Mono.E_FUN [
               Mono.M_RULE (
                 Mono.PATTY (Mono.P_VID ("g", Mono.VAR), Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT)),
                 Mono.EXPTY (
                   Mono.E_FUN [
                     Mono.M_RULE (
                       Mono.PATTY (Mono.P_VID ("x", Mono.VAR), Mono.T_UNIT),
                       Mono.EXPTY (
                         Mono.E_APP (
                           Mono.EXPTY (Mono.E_VID ("g", Mono.VAR), Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT)),
                           Mono.EXPTY (
                             Mono.E_APP (
                               Mono.EXPTY (Mono.E_VID ("f", Mono.VAR), Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT)),
                               Mono.EXPTY (Mono.E_VID ("x", Mono.VAR), Mono.T_UNIT)),
                             Mono.T_UNIT)),
                         Mono.T_UNIT))],
                   Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT)))],
             Mono.T_FUN (Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT), Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT))))],
       Mono.T_FUN (Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT), Mono.T_FUN (Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT), Mono.T_FUN (Mono.T_UNIT, Mono.T_UNIT)))))
    
  (* list.tml *)
  let list_tml : Mono.program =
    ([Mono.D_DTYPE;
    Mono.D_REC (
      Mono.PATTY (
        Mono.P_VID ("append", Mono.VAR), 
        Mono.T_FUN (Mono.T_NAME 0, Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0))), 
      Mono.EXPTY (
        Mono.E_FUN [
          Mono.M_RULE (
            Mono.PATTY (Mono.P_VID ("Nil", Mono.CON), Mono.T_NAME 0), 
            Mono.EXPTY (
              Mono.E_FUN [
                Mono.M_RULE (
                  Mono.PATTY (Mono.P_VID ("x", Mono.VAR), Mono.T_INT), 
                  Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_VID ("Cons", Mono.CONF), Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0), Mono.T_NAME 0)), Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_VID ("x", Mono.VAR), Mono.T_INT), Mono.EXPTY (Mono.E_VID ("Nil", Mono.CON), Mono.T_NAME 0)), Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0))), Mono.T_NAME 0))], 
              Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0)));
          Mono.M_RULE (
            Mono.PATTY (Mono.P_VIDP (("Cons", Mono.CONF), Mono.PATTY (Mono.P_PAIR (Mono.PATTY (Mono.P_VID ("h", Mono.VAR), Mono.T_INT), Mono.PATTY (Mono.P_VID ("t", Mono.VAR), Mono.T_NAME 0)), Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0))), Mono.T_NAME 0), 
            Mono.EXPTY (
              Mono.E_FUN [
                Mono.M_RULE (
                  Mono.PATTY (Mono.P_VID ("x", Mono.VAR), Mono.T_INT), 
                  Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_VID ("Cons", Mono.CONF), Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0), Mono.T_NAME 0)), Mono.EXPTY (Mono.E_PAIR (Mono.EXPTY (Mono.E_VID ("h", Mono.VAR), Mono.T_INT), Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_VID ("append", Mono.VAR) , Mono.T_FUN (Mono.T_NAME 0, Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0))), Mono.EXPTY (Mono.E_VID ("t", Mono.VAR), Mono.T_NAME 0)), Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0)), Mono.EXPTY (Mono.E_VID ("x", Mono.VAR), Mono.T_INT)), Mono.T_NAME 0)), Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0))), Mono.T_NAME 0))], 
              Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0)))],
        Mono.T_FUN (Mono.T_NAME 0, Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0))));
    Mono.D_REC 
      (Mono.PATTY 
       (Mono.P_VID ("reverse", Mono.VAR), 
        Mono.T_FUN (Mono.T_NAME 0, Mono.T_NAME 0)), 
       Mono.EXPTY 
       (Mono.E_FUN 
        [Mono.M_RULE 
           (Mono.PATTY (Mono.P_VID ("Nil", Mono.CON), Mono.T_NAME 0), 
          Mono.EXPTY (Mono.E_VID ("Nil", Mono.CON), Mono.T_NAME 0));
         Mono.M_RULE 
           (Mono.PATTY (Mono.P_VIDP (("Cons", Mono.CONF), Mono.PATTY (Mono.P_PAIR (Mono.PATTY (Mono.P_VID ("h", Mono.VAR), Mono.T_INT), Mono.PATTY (Mono.P_VID ("t", Mono.VAR) , Mono.T_NAME 0)) , Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0))) , Mono.T_NAME 0), 
          Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_VID ("append", Mono.VAR), Mono.T_FUN (Mono.T_NAME 0, Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0))), Mono.EXPTY (Mono.E_APP (Mono.EXPTY (Mono.E_VID ("reverse", Mono.VAR), Mono.T_FUN (Mono.T_NAME 0, Mono.T_NAME 0)), Mono.EXPTY (Mono.E_VID ("t", Mono.VAR), Mono.T_NAME 0)), Mono.T_NAME 0)), Mono.T_FUN (Mono.T_INT, Mono.T_NAME 0)), Mono.EXPTY (Mono.E_VID ("h", Mono.VAR) , Mono.T_INT)), Mono.T_NAME 0))], 
        Mono.T_FUN (Mono.T_NAME 0, Mono.T_NAME 0)));
    Mono.D_VAL 
      (Mono.PATTY (Mono.P_VID ("l", Mono.VAR), Mono.T_NAME 0), 
       Mono.EXPTY 
       (Mono.E_APP 
        (Mono.EXPTY (Mono.E_VID ("Cons", Mono.CONF), Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0), Mono.T_NAME 0)), 
         Mono.EXPTY 
           (Mono.E_PAIR 
            (Mono.EXPTY (Mono.E_INT 1, Mono.T_INT), 
             Mono.EXPTY 
             (Mono.E_APP 
              (Mono.EXPTY (Mono.E_VID ("Cons", Mono.CONF), Mono.T_FUN (Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0), Mono.T_NAME 0)), 
               Mono.EXPTY 
                 (Mono.E_PAIR 
                  (Mono.EXPTY (Mono.E_INT 2, Mono.T_INT), 
                   Mono.EXPTY (Mono.E_VID ("Nil", Mono.CON), Mono.T_NAME 0)),  
                Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0))), 
              Mono.T_NAME 0)), 
          Mono.T_PAIR (Mono.T_INT, Mono.T_NAME 0))),
        Mono.T_NAME 0) )],
     Mono.EXPTY 
     (Mono.E_APP 
      (Mono.EXPTY (Mono.E_VID ("reverse", Mono.VAR), Mono.T_FUN (Mono.T_NAME 0, Mono.T_NAME 0)), 
       Mono.EXPTY (Mono.E_VID ("l", Mono.VAR), Mono.T_NAME 0)), 
      Mono.T_NAME 0))
  end

module TestAll =
struct
let test name = 
  let program = Inout.read_file name in
  let _ = print_endline "Parse OK" in
  let _ = print_endline (Ast_print.program2str program) in
  
  let vprogram = Ast_valid.vprogram program in
  let _ = print_endline "Ast_valid.vprogram OK" in
  let _ = print_endline (Ast_print.program2str vprogram) in
  
  let tprogram = Typing.tprogram vprogram in
  let _ = print_endline "Typing.tprogram OK" in
  let _ = print_endline (Core_print.program2str tprogram) in
  
  let mprogram = Monomorphise.core2mono tprogram in
  let _ = print_endline "Monomorphise.core2mono OK" in
  let _ = print_endline (Mono_print.program2str mprogram) in

  let code = Translate.program2code mprogram in
  let _ = print_endline "Translate.program2code OK" in
  let _ = Inout.write_file (name^".exe") ("Machine code:\n"^(Mach.code2str code)) in
  
  let machine = Mach.createMachine code in
  let _ = Mach.execute (Inout.append_file (name^".exe")) machine in
  let _ = print_endline "Execution OK" in
  let _ = print_endline ("Please read "^name^".exe"^" for details.") in
  ()
end

module TestTyping =
struct
let test name = 
  let program = Inout.read_file name in
  let _ = print_endline "Parse OK" in
  let _ = print_endline (Ast_print.program2str program) in
  
  let vprogram = Ast_valid.vprogram program in
  let _ = print_endline "Ast_valid.vprogram OK" in
  let _ = print_endline (Ast_print.program2str vprogram) in
  
  let tprogram = Typing.tprogram vprogram in
  let _ = print_endline "Typing.tprogram OK" in
  let _ = print_endline (Core_print.program2str tprogram) in
  ()
end

module TestTranslate =
struct
let test mprogram = 
  let code = Translate.program2code mprogram in
  let _ = print_endline "Translate.program2code OK" in
  let _ = Inout.write_file "test.exe" ("Machine code:\n"^(Mach.code2str code)) in
  
  let machine = Mach.createMachine code in
  let _ = Mach.execute (Inout.append_file "test.exe") machine in
  let _ = print_endline "Execution OK" in
  let _ = print_endline "Please read \"test.exe\" for details." in
  ()
end
