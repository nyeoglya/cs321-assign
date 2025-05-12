(* label is a code label specified by LABEL instruction. *)
type label = string       (* code label *)

(* addr is an address. it is a code label (CADDR), a heap handle (HADDR),
     or an index into the machine stack (SADDR). *)
type addr =                 (* address *)
    CADDR of label          (* code address *)
  | HADDR of int            (* heap address *)    
  | SADDR of int            (* stack address *)

(* avalue is an atomic value; it is the content of a register or a memory 
     cell. avalue takes a single memory cell or a single register. *)
type avalue =               (* atomic value *)
    AINT of int             (* integer constant *)
  | ABOOL of bool           (* boolean constant *)
  | AUNIT                   (* unit *)
  | ASTR of string          (* string constant *)
  | AADDR of addr           (* adderss constant *)

(* there are numReg (= 32) registers. each register can be identified
     either with its mnemonic or with its raw register index. valid raw
     register indices are thus from 0 to (numReg - 1).

     SP (Stack Pointer), BP (Base Pointer), and ZR (Zero Registers) are 
     special registers. both SP and BP store only stack addresses. SP is
     automatically updated by PUSH and POP instructions and points to the top
     of the machine stack, which is always empty. BP is automatically updated
     by CALL and RETURN instructions. both SP and BP can be read from, but
     cannot be written to. ZR holds a zero and never changes its value.
     it takes no effects to write a avalue to ZR. 

     all the other registers are functionally equivalent and can hold 
     any avalue. 
     
     there is a speical purpose register PC (program counter). PC stores
     the current execution point. PC cannot be used in instruction operands. *)
type reg = int            (* register *)   
val numReg : int          (* number of registers *)
val sp : reg              (* = 0 *)
val bp : reg              (* = 1 *)
val cp : reg              (* = 2 *)
val ax : reg              (* = 3 *)
val bx : reg              (* = 4 *)
val cx : reg              (* = 5 *)
val dx : reg              (* = 6 *)
val ex : reg              (* = 7 *)
val fx : reg              (* = 8 *)
val gx : reg              (* = 9 *)
val r10 : reg             (* = 10 *)
val r11 : reg             (* = 11 *)
val r12 : reg             (* = 12 *)
val r13 : reg             (* = 13 *)
val r14 : reg             (* = 14 *)
val r15 : reg             (* = 15 *)
val r16 : reg             (* = 16 *)
val r17 : reg             (* = 17 *)
val r18 : reg             (* = 18 *)
val r19 : reg             (* = 19 *)
val r20 : reg             (* = 20 *)
val r21 : reg             (* = 21 *)
val r22 : reg             (* = 22 *)
val r23 : reg             (* = 23 *)
val r24 : reg             (* = 24 *)
val r25 : reg             (* = 25 *)
val r26 : reg             (* = 26 *)
val r27 : reg             (* = 27 *)
val r28 : reg             (* = 28 *)
val r29 : reg             (* = 29 *)
val tr : reg              (* = 30 *)
val zr : reg              (* = 31 *)

(* rvalue and lvalue are source operands and destination operands of
     machine instructions, respectively. rvalue evaluates to avalue by
     the machine whereas lvalue stores the destination of machine
     instructions. we denote the avalue of a rvalue r with val(r) and
     the destination of a lvalue l with loc(l). *)

type rvalue =               (* rvalue *)
    INT of int              (* integer constant *)             
  | BOOL of bool            (* boolean constant *)
  | UNIT                    (* unit *)
  | STR of string           (* string constant *)
  | ADDR of addr            (* address *)
  | REG of reg              (* register *)
  | REFADDR of addr * int   (* dereferencing with address and offset *)
    (* addr cannot be a code address. if addr is a heap address, int is the 
       offset within the heap chunk associated with the heap address, and
       it must be a non-negative interger less than the size of the heap 
       chunk. if addr is a stack address, int is the offset from the stack 
       address within the machine stack, and it can be a negative integer. *)
  | REFREG of reg * int     (* dereferencing with register and offset *)
    (* reg must hold an AADDR avalue. the same rule applies as in REFADDR in 
       dereferencing. *)

type lvalue =               (* lvalue *)
    LREG of reg             (* register *)
  | LREFADDR of addr * int  (* address with offset *)
    (* the same rule applies as in REFADDR in locating the destination. *)
  | LREFREG of reg * int    (* register with offset *)
    (* reg must hold an AADDR avalue. the same rule applies as in REFADDR
       in locating the destination. *)

(* state indicates the machine state. in EXECUTION, the instruction at the
     current execution point is executed at the next cycle. in either ABNORMAL
     or NORAML, no more instructions are executed. in NORMAL, the machine
     keeps an execution result. *)
type state = EXECUTION | ABNORMAL | NORMAL of avalue

(* instr is a machine instruction. instructions are executed sequentially:
     PC points to the next instruction after the execution of each instruction
     unless a jump takes place or either HALT or EXCEPTION is executed.

     each instruction takes up a single memory cell except for LABEL and DEBUG.

     MOVE (l, r) moves val(r) to loc(l).

     ADD (l, r1, r2) moves 'val(r1) + val(r2)' to loc(l). both val(r1) and
     val(r2) must be AINT avalues. SUB and MUL are defined in a similar way.

     XOR (l, r1, r2) moves 'val(r1) xor val(r2)' to loc(l). both val(r1) and
     val(r2) must be ABOOL avalues. 

     NOT (l, r) moves 'not val(r)' to loc(l). val(r) must be a ABOOL avalue.

     PUSH r moves val(r) to loc(LREFREG (SP, 0)) and increments SP by 1, or
     equivalently, pushes val(r) onto the stack.

     POP moves val(REFREG (SP, ~1)) to loc(l) and decrements SP by 1, or
     equivalently, pops the stack to loc(l).

     MALLOC (l, r) creates a new memory chunk of size val(r) in the machine
     heap, and moves its heap handle to loc(l). val(r) must be an AINT avalue.

     FREE r frees the heap chunk associated with val(r). val(r) must be 
     an AADDR avalue, and it must also be a heap address.

     LABEL declares a new label. it take no memory cells.

     JUMP r jumps to the code address represented by val(r). val(r) must be
     an AADDR avalue, and it must also be a code address.

     JMPNEQ (r1, r2, r3) jumps to the code address represented by val(r1) if
     val(r2) is not equal to val(r3). val(r1) must be an AADDR avalue, and it 
     must be a code address. both val(r2) and val(r3) must be AINT avalues.

     JMPNEQSTR (r1, r2, r3) jumps to the code address represented by val(r1) if
     val(r2) is not equal to val(r3). val(r1) must be an AADDR avalue, and it 
     must be a code address. both val(r2) and val(r3) must be ASTR avalues.

     JMPTRUE (r1, r2) jumps to the code address represented by val(r1) if
     val(r2) is a (ABOOL true) avalue. val(r1) must be an AADDR avalue, and 
     it must be a code address. val(r2) must be an ABOOL avalue. 

     CALL r pushs PC (the current execution point) onto the stack, pushes
     val(REG BP), moves val(REG SP) to loc(LREF BP), and jumps to the code 
     address represented by val(r). val(r) must an AADDR avalue, and it must 
     be a code address. 

     RETURN pops the stack onto loc(LREF BP), pops the stack onto PC, and 
     adjusts PC so that it points to the next instruction.

     HALT r changes the machine state to NORMAL with the execution result
     val(r).

     EXCEPTION changes the machine state to ABNORMAL.

     DEBUG is ignored. *)
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

(* code is a sequence of instructions to be executed by the machine. 
     if there are multiple LABEL instructions with the same label, the 
     last one counts and all the others are ignored. *)
type code = instr list

(* START_LABEL is the label in code from which the execution begins. *)
val start_label : label

(* functions to create new labels *)
(* labelNew () creates a new label.
   labelNewStr s creates a new label using s.
   labelNewLabel l s creates a new label using l and s. *)
val labelNew : unit -> label 
val labelNewStr : string -> label
val labelNewLabel : label -> string -> label

(* functions to create new code *)
(* code0 is empty code.
     clist il creates code from a list of instructions il.
     cpre il c creates code by prepending (clist il) to c.
     cpost c il creates code by appending (clist il) to c.
     (@@) c1 c2 creates code by concatenating c1 and c2. *)
val code0 : code
val clist : instr list -> code
val cpre : instr list -> code -> code
val cpost : code -> instr list -> code
val (@@) : code -> code -> code

(* abstract machine *)
type machine 

(* createMachine c creates a new machine from code c. PC is set to the 
     instruction containing START_LABEL. *)
val createMachine : code -> machine
(* execute f m executes the machine m until it terminates by executing
     either HALT or EXCEPTION. The execution result is written to the 
     stream f. *)
val execute : (string -> unit) -> machine -> avalue option

(* exceptions raised by createMachine and execute *)
exception CreateMachineError      (* cannot create a machine *)
exception CodeReadError           (* read code through rvalue *)
exception InvalidHeapAccessError  (* read a non-existent heap chunk *)
exception HeapOffsetError         (* heap offset out of bound *)
exception FreedHeapAccessError    (* read a freed heap chunk *)
exception DoubleFreeHeapError     (* free an already freed heap chunk *)
exception StackAccessError        (* stack access failure *)
exception RegisterAccessError     (* register index out of bound in reading *)
exception REFREGTypeError         (* type error in REFREG *)
exception WriteSPError            (* write an avalue to SP *)
exception WriteBPError            (* write an avalue to BP *)
exception InvalidRegisterError    (* register index out of boudn in writing *)
exception CodeWriteError          (* write to code *)
exception LREFREGTypeError        (* type error in LREFREG *)
exception IntTypeError            (* operand not an AINT avalue *)
exception BoolTypeError           (* operand not an ABOOL avalue *)
exception StringTypeError         (* operand not an ASTR avalue *)
exception StackOverflowError      (* stack overflow *)
exception StackUnderflowError     (* stack underflow *)
exception FreeTypeError           (* invalid FREE operand *)
exception JumpTypeError           (* invalid JUMP operand *)
exception NoLabelError            (* no label *)

(* code printing *)
val code2str : code -> string
