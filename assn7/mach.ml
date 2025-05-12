type label = string

type addr = 
    CADDR of label
  | HADDR of int
  | SADDR of int

type avalue =
    AINT of int
  | ABOOL of bool
  | AUNIT
  | ASTR of string
  | AADDR of addr

type reg = int
let numReg = 32
let (sp : reg) = 0
let (bp : reg) = 1 
let (cp : reg) = 2 
let (ax : reg) = 3
let (bx : reg) = 4
let (cx : reg) = 5
let (dx : reg) = 6
let (ex : reg) = 7
let (fx : reg) = 8
let (gx : reg) = 9
let (r10 : reg) = 10
let (r11 : reg) = 11
let (r12 : reg) = 12
let (r13 : reg) = 13
let (r14 : reg) = 14
let (r15 : reg) = 15
let (r16 : reg) = 16
let (r17 : reg) = 17
let (r18 : reg) = 18
let (r19 : reg) = 19
let (r20 : reg) = 20
let (r21 : reg) = 21
let (r22 : reg) = 22
let (r23 : reg) = 23
let (r24 : reg) = 24
let (r25 : reg) = 25
let (r26 : reg) = 26
let (r27 : reg) = 27
let (r28 : reg) = 28
let (r29 : reg) = 29
let (tr : reg) = 30 
let (zr : reg) = 31 

type rvalue =
    INT of int
  | BOOL of bool
  | UNIT
  | STR of string
  | ADDR of addr
  | REG of reg
  | REFADDR of addr * int
  | REFREG of reg * int

type lvalue =
    LREG of reg
  | LREFADDR of addr * int
  | LREFREG of reg * int

type state = EXECUTION | ABNORMAL | NORMAL of avalue

type instr =
    MOVE of lvalue * rvalue
  | ADD of lvalue * rvalue * rvalue
  | SUB of lvalue * rvalue * rvalue
  | MUL of lvalue * rvalue * rvalue
  | XOR of lvalue * rvalue * rvalue
  | NOT of lvalue * rvalue
  | PUSH of rvalue
  | POP of lvalue
  | MALLOC of lvalue * rvalue
  | FREE of rvalue
  | LABEL of label
  | JUMP of rvalue
  | JMPNEQ of rvalue * rvalue * rvalue
  | JMPNEQSTR of rvalue * rvalue * rvalue
  | JMPTRUE of rvalue * rvalue
  | CALL of rvalue
  | RETURN
  | HALT of rvalue
  | EXCEPTION 
  | DEBUG of string

type code = instr list

let (start_label : label) = "_Start_CSE_321_HW7_"

type machine = {
  register : avalue array;
  codem : instr array;
  coded : (label, int) Dict.dict;
  pc : int ref;
  heapm : (int, (bool ref * avalue array)) Dict.dict ref;
  heaps : int ref;
  stack : avalue array;
  state : state ref;
  maxStack : int ref;
  maxHeap : int ref;
  countHeap : int ref;
  countInstruction : int ref;
  countMemRead : int ref;
  countMemWrite : int ref;
  countRegRead : int ref;
  countRegWrite : int ref
}

let caddrCurrent = ref 0

let labelNew () = 
  (caddrCurrent := !caddrCurrent + 1; 
   string_of_int (!caddrCurrent))

let labelNewStr s = 
  (caddrCurrent := !caddrCurrent + 1; 
   "L_" ^ s ^ (string_of_int (!caddrCurrent)))

let labelNewLabel a s = 
  (caddrCurrent := !caddrCurrent + 1; 
   a ^ s ^ (string_of_int (!caddrCurrent)))
    
let code0 = []

let clist il = il

let cpre il code = il @ code

let cpost code il = code @ il

let (@@) code code' = code @ code'

(*
 * 2str functions
 *)

let rec r2str r = match r with 
    INT i -> string_of_int i
  |  BOOL b -> string_of_bool b
  |  UNIT -> "()"
  |  STR s -> "\"" ^ s ^ "\""
  |  ADDR (CADDR a) -> "&" ^ a
  |  ADDR (HADDR a) -> "&Heap_" ^ (string_of_int a)
  |  ADDR (SADDR a) -> "&Stack_" ^ (string_of_int a)
  |  REG 0 -> "SP"
  |  REG 1 -> "BP"
  |  REG 2 -> "CP"
  |  REG 3 -> "AX"
  |  REG 4 -> "BX"
  |  REG 30 -> "TR"
  |  REG 31 -> "ZR"
  |  REG r -> "R[" ^ (string_of_int r) ^ "]"
  |  REFADDR (CADDR a, i) -> "*(&" ^ a ^ " + " ^ (string_of_int i) ^ ")"
  |  REFADDR (HADDR a, i) -> "*(&Heap_" ^ (string_of_int a) ^ " + " ^ (string_of_int i) ^ ")"
  |  REFADDR (SADDR a, i) -> "*(&Stack_" ^ (string_of_int a) ^ " + " ^ (string_of_int i) ^ ")"
  |  REFREG (r, i) -> "*(" ^ (r2str (REG r)) ^ " + " ^ (string_of_int i) ^ ")"

let rec l2str l = match l with
    LREG 0 -> "SP"
  | LREG 1 -> "BP"
  | LREG 2 -> "CP"
  | LREG 3 -> "AX"
  | LREG 4 -> "BX"
  | LREG 30 -> "TR"
  | LREG 31 -> "ZR"
  | LREG r -> "R[" ^ (string_of_int r) ^ "]"
  | LREFADDR (CADDR a, i) -> "(&" ^ a ^ " + " ^ (string_of_int i) ^ ")"
  | LREFADDR (HADDR a, i) -> "(&Heap_" ^ (string_of_int a) ^ " + " ^ (string_of_int i) ^ ")"
  | LREFADDR (SADDR a, i) -> "(&Stack_" ^ (string_of_int a) ^ " + " ^ (string_of_int i) ^ ")"
  | LREFREG (r, i) -> "(" ^ (l2str (LREG r)) ^ " + " ^ (string_of_int i) ^ ")"

let instr2str instr = match instr with
    MOVE (l, r) -> "\tmove " ^ (l2str l) ^ " <- " ^ (r2str r)
  | ADD (l, r1, r2) -> 
     "\tadd " ^ (l2str l) ^ " <- (" ^ (r2str r1) ^ ", " ^ (r2str r2) ^ ")"
  | SUB (l, r1, r2) -> 
     "\tsub " ^ (l2str l) ^ " <- (" ^ (r2str r1) ^ ", " ^ (r2str r2) ^ ")"
  | MUL (l, r1, r2) -> 
     "\tmul " ^ (l2str l) ^ " <- (" ^ (r2str r1) ^ ", " ^ (r2str r2) ^ ")"
  | XOR (l, r1, r2) -> 
     "\txor " ^ (l2str l) ^ " <- (" ^ (r2str r1) ^ ", " ^ (r2str r2) ^ ")"
  | NOT (l, r) -> "\tnot " ^ (l2str l) ^ " <- " ^ (r2str r)
  | PUSH r -> "\tpush " ^ (r2str r)
  | POP l -> "\tpop " ^ (l2str l)
  | MALLOC (l, r) ->  "\tmalloc " ^ (l2str l) ^ " <- " ^ (r2str r)
  | FREE r -> "\tfree " ^ (r2str r)
  | LABEL a -> a ^ ":"
  | JUMP r -> "\tjump " ^ (r2str r)
  | JMPNEQ (r1, r2, r3) -> 
     "\tjmpneq " ^ (r2str r1) ^ ", (" ^ (r2str r2) ^ ", " ^ (r2str r3) ^ ")"
  | JMPNEQSTR (r1, r2, r3) -> 
     "\tjmpneqstr " ^ (r2str r1) ^ ", (" ^ (r2str r2) ^ ", " ^ (r2str r3) ^ ")"
  | JMPTRUE (r1, r2) -> 
     "\tjmptrue " ^ (r2str r1) ^ ", (" ^ (r2str r2) ^ ")"
  | CALL r -> "\tcall " ^ (r2str r)
  | RETURN -> "\treturn"
  | HALT r -> "\thalt " ^ (r2str r)
  | EXCEPTION  -> "\texception"
  | DEBUG s -> "\t// " ^ s

let rec code2str c = match c with 
    [] -> ""
  (* | (LABEL a)::tl -> (instr2str (LABEL a)) ^ (code2str tl) *) 
  | c::tl -> (instr2str c) ^ "\n" ^  (code2str tl)

let avalue2str av = match av with 
    AINT i -> string_of_int i
  | ABOOL b -> string_of_bool b
  | AUNIT -> "()"
  | ASTR s -> "\"" ^ s ^ "\""
  | AADDR a -> r2str (ADDR a)

(*
 * machine
 *)

exception CreateMachineError
exception CodeReadError
exception InvalidHeapAccessError
exception HeapOffsetError
exception FreedHeapAccessError
exception DoubleFreeHeapError
exception StackAccessError
exception RegisterAccessError
exception REFREGTypeError
exception WriteSPError
exception WriteBPError
exception InvalidRegisterError
exception CodeWriteError
exception LREFREGTypeError
exception IntTypeError
exception BoolTypeError
exception StringTypeError
exception StackOverflowError
exception StackUnderflowError
exception FreeTypeError
exception JumpTypeError
exception NoLabelError

exception Debug

let costRegRead = 1
let costRegWrite = 1
let costMemRead = 10
let costMemWrite = 10
let costInstruction = costRegRead + costMemRead

let stackSize = 1024 * 1024       (* stack size *)
                         
let createMachine code =
  try 
    let register = Array.make numReg (AINT 0) in
    let _ = register.(sp) <- AADDR (SADDR 0) in 
    let _ = register.(bp) <- AADDR (SADDR 0) in
    let _ = register.(zr) <- AINT 0 in
    let codem = Array.of_list code in
    let count_array = ref 0 in
    let coded = Array.fold_left
                  (fun coded' instr' -> 
                    let _ = count_array := !count_array+1 in
                    match instr' with
                      LABEL label' -> Dict.insert (label', !count_array-1) coded'
                    | _ -> coded')
                  Dict.empty codem in
    let pc = ref (match Dict.lookup start_label coded with
                    Some v -> v
                  | _ -> raise CreateMachineError) in
    let heapm = ref Dict.empty in 
    let heaps = ref 0 in 
    let stack = Array.make stackSize (AINT 0) in
    let state = ref EXECUTION 
    in
    {register = register; codem = codem; coded = coded;
     pc = pc; heapm = heapm; heaps = heaps; stack = stack; state = state;
     maxStack = ref 0;
     maxHeap = ref 0;
     countHeap = ref 0;
     countInstruction = ref 0;
     countMemRead = ref 0;
     countMemWrite = ref 0;
     countRegRead = ref 0;
     countRegWrite = ref 0
    }
  with _ -> raise CreateMachineError
                  
let execute fprint
            ({register = register; codem = codem; coded = coded; pc = pc; 
              heapm = heapm; heaps = heaps; stack = stack; state = state;
              maxStack = maxStack; 
              maxHeap = maxHeap;
              countHeap = countHeap;
              countInstruction = countInstruction;
              countMemRead = countMemRead;
              countMemWrite = countMemWrite;
              countRegRead = countRegRead;
              countRegWrite = countRegWrite
            }) = 

  let addc c i = c := !c + i in
  let incc c = c := !c + 1 in

  let readRegister r = 
    (try incc countRegRead;
         register.(r) with Invalid_argument "index out of bounds" -> raise RegisterAccessError) in
  
  let writeRegister r a = 
    (try register.(r) <- a with Invalid_argument "index out of bounds" -> raise InvalidRegisterError);
    incc countRegWrite in
  
  let readHeap h os =
    let (alive, mcell) = 
      match Dict.lookup h (!heapm) with 
        Some v -> v
      | None -> raise InvalidHeapAccessError
    in
    (incc countMemRead;
     if !alive then
       try mcell.(os) with Invalid_argument "index out of bounds" -> raise HeapOffsetError
     else 
       raise FreedHeapAccessError) in  

  let writeHeap h os a = 
    let (alive, mcell) = 
      match Dict.lookup h (!heapm) with 
        Some v -> v
      | None -> raise InvalidHeapAccessError
    in
    (if !alive then
       try mcell.(os) <- a with Invalid_argument "index out of bounds" -> raise HeapOffsetError
     else 
       raise FreedHeapAccessError);
    incc countMemWrite in

  let readStack s =
    (incc countMemRead;
     if s > !maxStack then maxStack := s else ();
     if s >= stackSize then raise StackOverflowError 
     else if s < 0 then raise StackUnderflowError
     else try stack.(s) with Invalid_argument "index out of bounds" -> raise StackAccessError) in
  
  let writeStack s a =
    (if s >= stackSize then raise StackOverflowError 
     else if s < 0 then raise StackUnderflowError
     else try stack.(s) <- a with Invalid_argument "index out of bounds" -> raise StackAccessError);
    incc countMemWrite;
    if s > !maxStack then maxStack := s else () in

  let showHeap () =
    (fprint "\n";
     Dict.fold
       (fun () (a, (br, aa)) -> 
        (List.iter fprint ["&Heap_"; string_of_int a; " ="];
         if !br then
           Array.iteri (fun i av -> 
                        List.iter fprint [" ["; string_of_int i; "] = "; avalue2str av])
                       aa
         else 
           fprint " <freed>";
         fprint "\n"))
       ()
       (!heapm)) in

  let r2a r = match r with 
      INT i -> AINT i
    | BOOL b -> ABOOL b
    | UNIT -> AUNIT
    | STR s -> ASTR s
    | ADDR a -> AADDR a
    | REG r -> register.(r)
    | REFADDR (CADDR _, _) -> raise CodeReadError
    | REFADDR (HADDR h, os) -> readHeap h os
    | REFADDR (SADDR s, os) -> readStack (s + os)
    | REFREG (r, os) ->
       match readRegister r with
         AADDR (CADDR _) -> raise CodeReadError
       | AADDR (HADDR h) -> readHeap h os
       | AADDR (SADDR s) -> readStack (s + os)
       | _ -> let _ = print_endline (r2str (REG r)^" : "^(avalue2str (readRegister r))^"\nCP : "^avalue2str (readRegister cp))
              (* let _ = showHeap () *)
              in raise REFREGTypeError in
  
  let a2l a l = match l with
      LREG r -> 
      if r = sp then raise WriteSPError
      else if r = bp then raise WriteBPError
      else if r = zr then () 
      else writeRegister r a 
    | LREFADDR (CADDR _, _) -> raise CodeWriteError
    | LREFADDR (HADDR h, os) -> writeHeap h os a
    | LREFADDR (SADDR s, os) -> writeStack (s + os) a
    | LREFREG (r, os) -> 
       match readRegister r with 
         AADDR (CADDR _) -> raise CodeWriteError
       | AADDR (HADDR h) -> writeHeap h os a
       | AADDR (SADDR s) -> writeStack (s + os) a
       | _ -> raise LREFREGTypeError in

  let incrPC () = pc := !pc + 1 in
  let setPC i = pc := i in

  let checkInt a1 a2 =
    match a1 with 
      AINT i1 -> (match a2 with AINT i2 -> (i1, i2) | _ -> raise IntTypeError)
    | _ -> raise IntTypeError in

  let checkBool a1 a2 =
    match a1 with 
      ABOOL b1 -> (match a2 with ABOOL b2 -> (b1, b2) | _ -> raise BoolTypeError)
    | _ -> raise BoolTypeError in

  let checkCodeAddr a =
    let l = match a with (AADDR (CADDR ca)) -> ca | _ -> raise JumpTypeError
    in
    try match Dict.lookup l coded with Some v -> v | _ -> raise NoLabelError
    with _ -> raise NoLabelError in

  let checkStr a1 a2 =
    match a1 with 
      ASTR s1 -> (match a2 with ASTR s2 -> (s1, s2) | _ -> raise StringTypeError)
    | _ -> raise StringTypeError in

  let showStack st = 
    (fprint "\n";
     fprint ("Stack(" ^ (string_of_int st) ^ ") = ");
     Array.iteri (fun i a -> 
                  if i < st then
                    List.iter fprint ["["; string_of_int i; "] = "; avalue2str a; "/"]
                  else 
                    ()) stack;
     fprint "\n") in
  
  let exe instr = match instr with
      MOVE (l, r) -> a2l (r2a r) l; incrPC ()
    | ADD (l, r1, r2) ->
       let (i1, i2) = checkInt (r2a r1) (r2a r2)
       in
       a2l (AINT (i1 + i2)) l; 
       incrPC ()
              
    | SUB (l, r1, r2) ->
       let (i1, i2) = checkInt (r2a r1) (r2a r2)
       in
       a2l (AINT (i1 - i2)) l; 
       incrPC ()
              
    | MUL (l, r1, r2) ->
       let (i1, i2) = checkInt (r2a r1) (r2a r2)
       in
       a2l (AINT (i1 * i2)) l; 
       incrPC ()
              
    | XOR (l, r1, r2) ->
       let (b1, b2) = checkBool (r2a r1) (r2a r2)
       in
       a2l (ABOOL ((b1 && not b2) || (not b1 && b2))) l; 
       incrPC ()
              
    | NOT (l, r) ->
       let (b, _) = checkBool (r2a r) (ABOOL true)
       in
       a2l (ABOOL (not b)) l; 
       incrPC ()
              
    | PUSH r -> 
       let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug
       in
       writeStack st (r2a r); 
       writeRegister sp (AADDR (SADDR (st + 1))); 
       incrPC ()
              
    | POP l ->
       let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug
       in
       writeRegister sp (AADDR (SADDR (st - 1))); 
       a2l (readStack (st - 1)) l;
       incrPC ()
              
    | MALLOC (l, r) ->
       let (size, _) = checkInt (r2a r) (AINT 0)
       in
       heapm := Dict.insert (!heaps, (ref true, Array.make size (AINT 0))) (!heapm);
       a2l (AADDR (HADDR (!heaps))) l;
       heaps := !heaps + 1;
       addc countHeap size;
       if !countHeap > !maxHeap then maxHeap := !countHeap else ();
       incrPC ()
              
    | FREE r ->
       let h = match (r2a r) with AADDR (HADDR ha) -> ha | _ -> raise FreeTypeError in
       let freeHeap h =
         let (alive, mcell) = 
           match Dict.lookup h (!heapm) with 
             Some v -> v
           | None -> raise InvalidHeapAccessError
         in
         if !alive then alive := false else raise DoubleFreeHeapError;
         addc countHeap (-(Array.length mcell))
              
       in
       freeHeap h;
       incrPC ()
              
    | LABEL _ -> incrPC (); addc countInstruction (-1)
    | JUMP r -> setPC (checkCodeAddr (r2a r))
    | JMPNEQ (r1, r2, r3) ->
       let i1 = checkCodeAddr (r2a r1) in
       let (i2, i3) = checkInt (r2a r2) (r2a r3)
       in
       if i2 = i3 then incrPC ()
       else setPC i1
                  
    | JMPNEQSTR (r1, r2, r3) ->
       let i1 = checkCodeAddr (r2a r1) in
       let (s2, s3) = checkStr (r2a r2) (r2a r3)
       in
       if s2 = s3 then incrPC ()
       else setPC i1
                  
    | JMPTRUE (r1, r2) ->
       let i1 = checkCodeAddr (r2a r1) in
       let (b2, _) = checkBool (r2a r2) (ABOOL true)
       in
       if b2 then setPC i1
       else incrPC ()
                   
    | CALL r ->
       let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug in
       let i = checkCodeAddr (r2a r)
       in
       writeStack st (readRegister bp);
       writeStack (st + 1) (AINT (!pc));
       writeRegister bp (AADDR (SADDR (st + 2)));
       writeRegister sp (AADDR (SADDR (st + 2)));
       setPC i
             
    | RETURN ->
       let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug in
       (* let _ = showStack st *)
       let (i, _) = checkInt (readStack (st - 1)) (AINT 0)
       in
       writeRegister sp (AADDR (SADDR (st - 2)));
       writeRegister bp (readStack (st - 2));
       setPC (i + 1)
             
    | HALT r -> state := NORMAL (r2a r)
    | EXCEPTION -> state := ABNORMAL
    | DEBUG s -> incrPC (); addc countInstruction (-1) in
  
  let codeSize = Array.fold_left 
                   (fun c l -> match (l,c) with
                                 (LABEL _, cs) -> cs
                               | (DEBUG _, cs) -> cs
                               | (_, cs) -> cs + 1)
                   0 codem in

  let showStatistics () = 
    List.iter fprint ["\n"; "Execution statistics"; "\n"];
    List.iter fprint ["\tcode size = "; string_of_int codeSize; "\n"];
    List.iter fprint ["\tmax stack = "; string_of_int (!maxStack); "\n"];
    List.iter fprint ["\tmax heap = "; string_of_int (!maxHeap); "\n"];
    List.iter fprint ["\tinstructions executed = "; string_of_int (!countInstruction); "\n"];
    List.iter fprint ["\tmemory read = "; string_of_int (!countMemRead); "\n"];
    List.iter fprint ["\tmemory write = "; string_of_int (!countMemWrite); "\n"];
    List.iter fprint ["\tregister read = "; string_of_int (!countRegRead); "\n"];
    List.iter fprint ["\tregister write = "; string_of_int (!countRegWrite); "\n"];
    List.iter fprint ["Total time cost = "; 
                      string_of_int (
                          (!countMemRead) * costMemRead +
                            (!countMemWrite) * costMemWrite +
                            (!countRegRead) * costRegRead +
                            (!countRegWrite) * costRegWrite +
                            (!countInstruction) * costInstruction); 
                      "\n"];
    List.iter fprint ["Total memory cost = "; 
                      string_of_int ((!maxStack) + (!maxHeap) + codeSize); "\n"] in

  let rec avalue2str a = match a with 
      AINT i -> string_of_int i
    | ABOOL b -> string_of_bool b
    | AUNIT -> "()"
    | ASTR s -> String.concat "" ["\""; s; "\""]
    | AADDR (CADDR label) -> String.concat "" ["LABEL_"; label]
    | AADDR (HADDR ha) ->
       let (alive, mcell) =
         match Dict.lookup ha (!heapm) with
           Some v -> v
         | None -> raise InvalidHeapAccessError
       in
       if !alive then
         String.concat ""
                       ["HEAP_&"; string_of_int ha; ": [";
                        Array.fold_right
                          (fun avalue str -> String.concat "" ["|"; avalue2str avalue; "|"; str]) mcell "";
                        "]"]
       else raise FreedHeapAccessError
    | AADDR (SADDR sa) ->
       if sa >= stackSize then raise StackOverflowError else
         if sa < 0 then raise StackUnderflowError else
           String.concat ""
                         ["STACK_&"; string_of_int sa; ": ["; 
                          avalue2str (stack.(sa));
                          "]"] in
  
  let _ = List.iter fprint ["\n\nExecution begins at "; string_of_int (!pc); ":\n"] in
  
  let rec loop () = 
    let instr = codem.(!pc) in
    let _ = List.iter fprint [string_of_int (!pc); "   ";  instr2str instr; "\n"] in
    
    let _ = exe instr in
    let _ = incc countInstruction in
    
    match !state with
      EXECUTION -> loop ()
    (*| ABNORMAL -> (showStatistics () ;fprint "\nAbnormal termination\n"; None)*)
    | ABNORMAL -> (
        let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug in
        let _ = showStack st in
        let _ = showHeap () in
        showStatistics ();
        fprint "\nAbnormal termination\n"; None
    )
    | NORMAL a -> 
       let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug in
       let _ = showStack st in
       let _ = showHeap () in
       let _ = showStatistics () in
       let _ = List.iter fprint ["\nNormal termination"; "\n"] in
       let _ = List.iter fprint ["\tResult = "; avalue2str a; "\n"] in
       Some a
  in
  (*try loop () with _ -> List.iter fprint ["\nAbnormal execution"; "\n"]; None*)
  try loop () with _ ->
    let st = match readRegister sp with AADDR (SADDR i) -> i | _ -> raise Debug in
    let _ = showStack st in
    let _ = showHeap () in
    List.iter fprint ["\nAbnormal execution"; "\n"]; None
                                                                           
