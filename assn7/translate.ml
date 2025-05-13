open Mach
open Mono 
open Mono_print

exception NotImplemented

(* location *)
type loc =
  | L_INT of int          (* integer constant *)
  | L_BOOL of bool        (* boolean constant *)
  | L_UNIT                (* unit constant *)
  | L_STR of string       (* string constant *)
  | L_ADDR of Mach.addr   (* at the specified address *)
  | L_REG of Mach.reg     (* at the specified register *)
  | L_DREF of loc * int   (* at the specified location with the specified offset *)

type venv = (Mono.avid, loc) Dict.dict  (* variable environment *)
let venv0 : venv = Dict.empty           (* empty variable environment *)

type env = venv * int
let env0 : env = (venv0, 0)

(* val loc2rvalue : loc -> Mach.code * rvalue *)
let rec loc2rvalue l = match l with
  | L_INT i -> (Mach.code0, Mach.INT i)
  | L_BOOL b -> (Mach.code0, Mach.BOOL b)
  | L_UNIT -> (Mach.code0, Mach.UNIT)
  | L_STR s -> (Mach.code0, Mach.STR s)
  | L_ADDR a -> (Mach.code0, Mach.ADDR a)
  | L_REG r -> (Mach.code0, Mach.REG r)
  | L_DREF (L_ADDR a, i) -> (Mach.code0, Mach.REFADDR (a, i))
  | L_DREF (L_REG r, i) -> (Mach.code0, Mach.REFREG (r, i))
  | L_DREF (l, i) ->
     let (code, rvalue) = loc2rvalue l in
     (Mach.cpost code [Mach.MOVE (Mach.LREG Mach.tr, rvalue)], Mach.REFREG (Mach.tr, i))

(*
 * helper functions for debugging
 *)
(* val loc2str : loc -> string *)
let rec loc2str l = match l with 
  | L_INT i -> "INT " ^ (string_of_int i)
  | L_BOOL b -> "BOOL " ^ (string_of_bool b)
  | L_UNIT -> "UNIT"
  | L_STR s -> "STR " ^ s
  | L_ADDR (Mach.CADDR a) -> "ADDR " ^ ("&" ^ a)
  | L_ADDR (Mach.HADDR a) -> "ADDR " ^ ("&Heap_" ^ (string_of_int a))
  | L_ADDR (Mach.SADDR a) -> "ADDR " ^ ("&Stack_" ^ (string_of_int a))
  | L_REG r -> 
     if r = Mach.sp then "REG SP"
     else if r = Mach.bp then "REG BP"
     else if r = Mach.cp then "REG CP"
     else if r = Mach.ax then "REG AX"
     else if r = Mach.bx then "REG BX"
     else if r = Mach.tr then "REG TR"
     else if r = Mach.zr then "REG ZR"
     else "R[" ^ (string_of_int r) ^ "]"
  | L_DREF (l, i) -> "DREF(" ^ (loc2str l) ^ ", " ^ (string_of_int i) ^ ")"

(* rvalue2loc : Mach.rvalue -> loc *)
let rvalue2loc rv = match rv with
  | INT i -> L_INT i
  | BOOL b -> L_BOOL b
  | UNIT -> L_UNIT
  | STR s -> L_STR s
  | ADDR a -> L_ADDR a
  | REG r -> L_REG r
  | REFADDR (a, i) -> L_DREF (L_ADDR a, i)
  | REFREG (r, i) -> L_DREF (L_REG r, i)

(*
 * Generate code for Abstract Machine MACH 
 *)

let unwarp x = match x with
  | Some v -> v
  | _ -> raise NotImplemented

module SLoc = struct
  type t = { mutable v : int }
  let create i = { v = i }
  let get c = c.v
  let set c v = c.v <- v
  let add c = c.v <- c.v + 1
  let sub c = c.v <- c.v - 1
end

let sloc = SLoc.create (-1)

(* pat2code : Mach.label -> Mach.label - > loc -> Mono.pat -> Mach.code * venv *)
let rec pat2code ls lf lo p = match p with
  | P_WILD -> ([LABEL ("pat_wild" ^ ls)], venv0)
  | P_UNIT -> ([LABEL ("pat_unit" ^ ls)], venv0)
  | P_INT i ->
      let (co, rv) = loc2rvalue lo in
      let jco = [
        MOVE (LREG r25, rv);
        MOVE (LREG r25, REFREG (r25, 0));
        JMPNEQ (ADDR (CADDR lf), REG r25, INT i);
      ] in ([LABEL ("pat_int" ^ ls)] @ co @ jco, venv0)
  | P_BOOL b ->
      let (co, rv) = loc2rvalue lo in
      let co' = [
        MOVE (LREG r25, rv);
        MOVE (LREG r25, REFREG (r25, 0));
        XOR (LREG r25, REG r25, BOOL b);
        JMPTRUE (ADDR (CADDR lf), REG r25)
      ]
    in ([LABEL ("pat_bool" ^ ls)] @ co @ co', venv0)
  | P_VID v ->
      let (co, rv) = loc2rvalue lo in (
        match v with
          | (avid, VAR) -> ([LABEL ("pat_var" ^ ls)] @ co, Dict.singleton (avid, lo)) (*재귀에서 이거 이름 안겹치나? -> 어차피 함수 안으로 들어갈 때만 더해지는 거니까 상관이 없는 듯 하다.*)
          | (avid, CON) ->
              let cco = [
                MOVE (LREG r25, rv);
                MOVE (LREG r25, REFREG (r25, 0));
                JMPNEQSTR (ADDR (CADDR lf), REG r25, STR avid)
              ] in ([LABEL ("pat_vid" ^ ls)] @ co @ cco, venv0)
          | _ -> ([LABEL ls], venv0)
      )
  | P_VIDP (v, PATTY (py, t)) ->
      let (co, rv) = loc2rvalue lo in (
        match v with
          | (avid, CONF) ->
              let aco = [
                MOVE (LREG r25, rv);
                MOVE (LREG r25, REFREG (r25, 0)); (*P_VIDP는 2번째 위치에 값이 있지만 나머지 데이터들은 그런게 없다. 즉, 나머지 데이터에서 2번째 값을 불러오려고 하면 당연히 문제가 생긴다. 이에, conf_func에서는 순서를 뒤집는다.*)
                JMPNEQSTR (ADDR (CADDR lf), REG r25, STR avid)
              ] in let rco, ren = pat2code (labelNew ()) lf (L_DREF (lo, 1)) py (*r27이 문제인건 가히 확실하다*)
              (*TODO: r27은 바뀔 수도 있기 때문에 이것을 스택으로 변경해야 한다. 일반 함수에서는 문제가 크게 드러나지 않을 수도 있지만 같은 함수를 여러번 실행하는 재귀 함수에서는 바로 문제가 드러난다.*)
              (*여기서 뭔가가 문제가 있는 것은 거의 확실하다. 그러나, 왜 여기서 문제가 생기는지는 아직도 잘 모르겠음.*)
            in ([LABEL ("pat_vidp" ^ ls)] @ co @ aco @ rco, ren)
          | _ -> ([LABEL ls], venv0)
      )
  | P_PAIR (PATTY (lp, t1), PATTY (rp, t2)) ->
      let (co, rv) = loc2rvalue lo in (*해당 위치에 있는 rvalue를 가져온다*)
      let (aco, al) = ([], lo) (*계속 고민해봤는데 이게 왜 잘 작동하는지 아직도 모르겠다.*)
      (*( match rv with (*그 rvalue를 분석하는데.*)
          | REFADDR r
          | REFREG r -> (*만약 rvalue가 reference라면*)
            ([ (*매칭된 값을 추적하기 위해 스택에 rv를 새로이 넣고 그걸 push한다.*)
              (*그러나, 이걸로 인해 sloc 값이 이상하게 변했고, 이로 인해 환경이 잘못되었다.*)
              (*ADD (LREG r17, REG r17, INT 1);*) (*이 새끼 때문에 다 꼬인거였다.*)
              MOVE (LREG ax, rv);
            ], L_DREF (L_REG bp, SLoc.get sloc))
          | _ -> ([], lo)
      )*)
    (*
      문제: 패턴매칭과 재귀를 동시에 쓰는 경우, 2번 이상 재귀가 돌아가면 문제가 발생한다. -> 환경이 꼬이는 문제가 발생함. 환경을 한번 전수조사를 해봐야할 것 같은데...
      원인1: 재귀적으로 실행할 때 함수가 끝나기 전에는 패턴을 계속 push되어있는 상태로 들고 있어야 되는데, 지금 코드 구현상으로는 R17이 0이 될 때까지 POP하고 있어 패턴이 실패하는 경우가 생기는 순간 과거에 넣어서 아직은 들고 있었어야 했던 값들까지 전부 POP해버린다...
      원인2: 환경 상으로는 아직 빼질 때가 아니라서 숫자를 가지고 있겠지만 실제 구현 상으로는 POP되어서 환경과 괴리가 생긴다. 이로 인해, 스택에 들어가는 값들이 몇개씩 밀리게 되고 문제가 크게 발생한다.
      해결책: 값을 더 완전히 추적할 수 있도록 변경해야 한다.
    *)
    in
      let lco, len = pat2code (labelNew ()) lf (L_DREF (al, 0)) lp in
      let rco, ren = pat2code (labelNew ()) lf (L_DREF (al, 1)) rp
    in ([LABEL ("pat_pair" ^ ls)] @ co @ aco @ lco @ rco, Dict.merge len ren)

let rec boundpy py = match py with
  | PATTY (P_VID (v, VAR), t) -> [v]
  | PATTY (P_VIDP ((v, CONF), py), t) -> v :: (boundpy py)
  | PATTY (P_PAIR (py1, py2), t) -> (boundpy py1) @ (boundpy py2)
  | _ -> []

(*TODO: 이 함수 decfreevar 안으로 집어넣기*)
let rec bounddec de = match de with (*dec의 bound되는 값들을 가져온다.*)
  | D_VAL (py, ey) -> boundpy py
  | D_REC (py, ey) -> boundpy py
  | _ -> []

let rec decfreevar (b: Mono.avid list) (de) : Mono.avid list =
  let bde = bounddec de in
    match de with
      | D_VAL (py, ey) -> List.filter (fun x -> if (List.mem x bde) then false else true) (eyfreevar (b @ (boundpy py)) ey)
      | D_REC (py, ey) -> List.filter (fun x -> if (List.mem x bde) then false else true) (eyfreevar (b @ (boundpy py)) ey)
      | _ -> []
and eyfreevar (b: Mono.avid list) (ey) : Mono.avid list = match ey with
  | EXPTY (e, t) -> (
    match e with
      | E_VID (v, VAR) -> if List.mem v b then [] else [v]
      | E_FUN ml -> mlfreevar2 b ml
      | E_APP (ey1, ey2) -> (eyfreevar b ey1) @ (eyfreevar b ey2)
      | E_PAIR (ey1, ey2) -> (eyfreevar b ey1) @ (eyfreevar b ey2)
      | E_LET (de, ey) -> (decfreevar b de) @ (eyfreevar b ey)
      | _ -> []
    )
and mlfreevar2 (b: Mono.avid list) (ml) : Mono.avid list = match ml with
  | [] -> []
  | M_RULE (py, ey) :: ml' ->
      let bpy = boundpy py in
      let efv = eyfreevar (b @ bpy) ey in
      let filtered = List.filter (fun x -> not (List.mem x bpy)) efv
    in filtered @ (mlfreevar2 b ml')
let mlfreevar (ml) : Mono.avid list = mlfreevar2 [] ml

(* exp2code : venv -> Mach.label -> Mono.exp -> Mach.code * Mach.rvalue *)
let rec exp2code (en: venv) (ls: Mach.label) (e: Mono.exp) : Mach.code * Mach.rvalue = match e with
  | E_INT i ->
    let _ = SLoc.add sloc in ([ (*stack 카운터 1 증가*)
      LABEL ls; (*시작지점 표시*)
      MALLOC (LREG ax, INT 1); (*공간 1개 할당*)
      MOVE (LREFREG (ax, 0), INT i); (*공간에 값 대입*)
      PUSH (REG ax); (*stack에 저장해서 추적*)
    ], REFREG (bp, SLoc.get sloc)) (*저장한 숫자를 가리키는 포인터를 반환.*)
  | E_BOOL b ->
    let _ = SLoc.add sloc in ([ (*stack 카운터 1 증가*)
      LABEL ls; (*시작지점 표시*)
      MALLOC (LREG ax, INT 1); (*공간 1개 할당*)
      MOVE (LREFREG (ax, 0), BOOL b); (*공간에 값 대입*)
      PUSH (REG ax); (*stack에 저장해서 추적*)
    ], REFREG (bp, SLoc.get sloc))
  | E_UNIT ->
    let _ = SLoc.add sloc in ([ (*stack 카운터 1 증가*)
      LABEL ls; (*시작지점 표시*)
      MALLOC (LREG ax, INT 1); (*공간 1개 할당*)
      MOVE (LREFREG (ax, 0), UNIT); (*공간에 값 대입*)
      PUSH (REG ax); (*stack에 저장해서 추적*)
    ], REFREG (bp, SLoc.get sloc))
  | E_VID v -> ( (*변수 값 다루기*)
    match v with
      | (avid, VAR) -> (*var는 ADDR이다.*)
          let vlo = unwarp (Dict.lookup avid en) in (*환경에서 변수 찾고 그것의 위치를 반환한다.*)
          let co, rv = loc2rvalue vlo in (*환경에서 변수 이름 찾고, code와 변수의 위치인 rvalue 쌍을 가져온다.*)
          let clc = [
            MOVE (LREG ax, rv); (*값을 가져왔다면 응당 ax에 저장해야 하는 법이다.*)
          ]
        in ([LABEL ls] @ co @ clc, rv) (*해당하는 값을 반환한다. 이미 stack에 저장이 되어 있는 값일 터이니(환경에 저장이 되어있다는 것은 이미 스택에 들어가있는 값이라는 뜻) 스택에 값을 다시 저장할 필요는 없어보인다.*)
      | (avid, CON) ->
        let _ = SLoc.add sloc in
        let co = [
          MALLOC (LREG r29, INT 1); (*공간 1개 할당*)
          MOVE (LREFREG (r29, 0), STR avid); (*그 공간에 constructor avid 저장*)
          PUSH (REG r29); (*그 공간을 stack에 저장. 나중에 패턴매칭할 때 이 포인터 가져와서 비교하는데 쓸거임!!!*)
          MOVE (LREG ax, REG r29) (*저장된 값이 코드의 최종 산물이기에 응당 ax에 저장한다.*)
        ] in ([LABEL ls] @ co, REFREG (bp, SLoc.get sloc))
      | (avid, CONF) ->
        let _ = SLoc.add sloc in
        let co = [
          MALLOC (LREG r29, INT 2); (*공간 2개 할당*)
          MOVE (LREFREG (r29, 1), STR avid); (*2번째 공간에 avid 저장*)
          MOVE (LREFREG (r29, 0), ADDR (CADDR "conf_func")); (*1번째 공간을 미리 정의된 함수를 채운다. 나중에 APP할 때에 conf_fun을 실행해서 값을 가져온다.*)
          PUSH (REG r29); (*해당 공간을 stack에 저장*)
          MOVE (LREG ax, REG r29) (*저장된 값이 코드의 최종 산물이기에 응당 ax에 저장한다.*)
        ] in ([LABEL ls] @ co, REFREG (bp, SLoc.get sloc))
  )
  | E_FUN ml -> (*함수 만들기.*)
      let fv = mlfreevar ml in (*free variable 찾기. 이걸 전부 가지고 있어야 함수가 값을 돌려줄 수 있음. 잘 만들어진 코드라면 각각은 이미 환경 어딘가에 저장이 되어 있을거임.*)
      let _ = SLoc.add sloc in (*스택에 함수 넣어야 되기 때문에 일단 스택 카운터 증가*)
      let fna = labelNew () in (*만들어지는 함수 이름*)
      let fsl = SLoc.get sloc in (*스택 카운터가 함수 만들 때 변할 수 있기 때문에 고정된 스택 카운터 값을 일단 하나 저장*)
      let _ = SLoc.set sloc (-1) in (*함수 내부에서는 스택 카운터가 처음부터 다시 시작해야 한다.*)
      let fpc = [
        MALLOC (LREG r29, INT 2); (*공간 2개 할당*)
        MOVE (LREFREG (r29, 0), ADDR (CADDR fna)); (*1번째 공간에 함수의 코드 상 위치 넣기. 나중에 꺼내서 쓸 수 있게.*)
        PUSH (REG r29) (*해당 공간을 stack에 저장. 반환하는 rvalue가 이거의 위치여야 한다!!!*)
      ] in let clo: code = if fv = [] then code0 else [ (*클로저 만들기.*)
        MALLOC (LREFREG (r29, 1), INT (List.length fv)); (*free variable 개수만큼 공간 추가 할당.*)
        MOVE (LREG r28, REFREG (r29, 1)) (*일단 free var 저장하는 곳으로 이동.*)
      ] in let rec cofv fv en re i = ( (*free var 받아서 클로저 만드는 함수. 함수 내부의 함수도 새 환경에서 다시 클로저 만들면서 잘 매핑되기에 역시 환경으로 처리가 가능하다.*)
        match fv with
          | [] -> code0 (*남은 free var가 없다면? -> 그냥 빈 코드 반환*)
          | x :: fv' -> (*free var x 1개 꺼냄*)
              let vlo = unwarp (Dict.lookup x en) in (*x를 환경에서 찾고 그 위치를 저장.*)
              let co, rv = loc2rvalue vlo (*해당하는 rvalue를 heap에서 가져옴.*)
            in (MOVE (LREFREG (re, i), rv)) :: (cofv fv' en re (i+1))
      ) in let enco = cofv fv en r28 0 in (*환경에서 변수 찾아서 매칭시켜서 코드 상의 closure를 만드는 부분.*)
      (*여기가 클로저를 통해 환경을 새로 만드는 곳이다. 클로저 환경 + 기존 환경으로 새 환경을 넘겨주어야 값 매칭이 된다.
      en이 있기 때문에 기존 환경에도 모든 데이터가 들어가 있지만, 현재 구현상 bp의 변경에 기존 환경이 대응할 수가 없기 때문에 클로저 값 전부 가져와서 PUSH하고 그걸로 환경 새로 만든 다음에(새 위치로 덮어쓰기) 하위 함수나 표현식에만 새 환경을 적용하는 방식으로 해결한다.*)
      let rec nen fv i = (
        match fv with
          | [] -> venv0
          | x :: fv' -> (*해당하는 avid를 free var에서 가져옴.*)
            Dict.merge (nen fv' (i+1)) (Dict.singleton (x, L_DREF (L_REG bp, i + 1))) (*클로저의 값들을 전부 PUSH하여 bp 값을 가져왔을 때 포인터 깊이가 증가하지 않는다. 함수 매개변수도 PUSH하기 때문에 +1 만큼 보정해줘야 함.*)
      ) in let ench = nen fv 0 in
      let fco = mlist2inco fna (Dict.merge en ench) ml (List.length fv) in (*함수 내부를 코드로 변환한다. 일부 en 요소(클로저 내부)를 ench로 바꿔치기한다. 잠재적으로 스택 카운터가 변할 수 있는 위험한 곳이지만, 스택 카운터 초기화했기 때문에 영향은 없다.*)
      let jpl = labelNew () in
      let fjp = [ (*함수 윗쪽과 아랫쪽에 점프를 추가해서 의도치 않게 함수 코드로 슬금슬금 넘어가는 것을 방지한다. 어셈블리어는 모든 코드를 순차적으로 실행하기 때문에 이런 문제가 발생할 수 있음.*)
        JUMP (ADDR (CADDR jpl));
      ] @ fco @ [
        LABEL jpl;
        MOVE (LREG ax, REFREG (bp, fsl)) (*모든 결과는 ax에 저장한다. 이건 함수도 마찬가지. 혹시 안쓸수도 있지만 일단 그냥 저장하고 보는 거임.*)
      ] in
      let _ = SLoc.set sloc fsl (*스택 카운터를 되돌려놓는다.*)
    in ([LABEL ls] @ fpc @ clo @ enco @ fjp, REFREG (bp, fsl)) (*함수 클로저 공간 할당 -> 거기다가 클로저 만들기 -> 함수 내부도 만들어서 붙이기*)
  | E_APP (EXPTY (e1, t1), EXPTY(e2, t2)) -> ( (*함수 app을 다룬다.*)
    match e1, e2 with
      | E_PLUS, E_PAIR (_, _)
      | E_MINUS, E_PAIR (_, _)
      | E_MULT, E_PAIR (_, _)
      | E_EQ, E_PAIR (_, _)
      | E_NEQ, E_PAIR (_, _) ->
          let E_PAIR (EXPTY (e21, t21), EXPTY (e22, t22)) = e2 in (*일단 왼쪽이 연산, 오른쪽이 pair라면 오른쪽의 각 부분을 쪼갠다.*)
          let plco, lrv = exp2code en (labelNew ()) e21 in (*왼쪽을 먼저 코드로 변환한다.*)
          let prco, rrv = exp2code en (labelNew ()) e22 (*그 다음에 오른쪽을 코드로 변환한다.*)
        in (
          match e1 with (*연산 종류를 매칭. 일단 위의 5개 중 하나임은 보장됨.*)
            | E_PLUS -> (*숫자 덧셈 연산*)
              let _ = SLoc.add sloc in
              let pco = [
                MOVE (LREG r21, lrv);
                MOVE (LREG r22, rrv);
                ADD (LREG ax, REFREG (r21, 0), REFREG (r22, 0)); (*왼쪽과 오른쪽의 rvalue를 통해 값을 더하고 ax에 저장한다(결과 저장).*)
                MALLOC (LREG r29, INT 1); (*더한 값을 새로운 공간을 할당해서 저장한다.*)
                MOVE (LREFREG (r29, 0), REG ax);
                PUSH (REG r29);
                MOVE (LREG ax, REG r29)
              ] in ([LABEL ls] @ plco @ prco @ pco, REFREG (bp, SLoc.get sloc))
            | E_MINUS -> (*숫자 뺄셈 연산*)
              let _ = SLoc.add sloc in
              let pco = [
                MOVE (LREG r21, lrv);
                MOVE (LREG r22, rrv);
                SUB (LREG ax, REFREG (r21, 0), REFREG (r22, 0));
                MALLOC (LREG r29, INT 1);
                MOVE (LREFREG (r29, 0), REG ax);
                PUSH (REG r29);
                MOVE (LREG ax, REG r29)
              ] in ([LABEL ls] @ plco @ prco @ pco, REFREG (bp, SLoc.get sloc))
            | E_MULT -> (*숫자 곱셈 연산*)
              let _ = SLoc.add sloc in
              let pco = [
                MOVE (LREG r21, lrv);
                MOVE (LREG r22, rrv);
                MUL (LREG ax, REFREG (r21, 0), REFREG (r22, 0));
                MALLOC (LREG r29, INT 1);
                MOVE (LREFREG (r29, 0), REG ax);
                PUSH (REG r29);
                MOVE (LREG ax, REG r29)
              ] in ([LABEL ls] @ plco @ prco @ pco, REFREG (bp, SLoc.get sloc))
            | E_EQ -> (*= 연산*)
              let nl1 = labelNew () in
              let nl2 = labelNew () in
              let _ = SLoc.add sloc in
              let pco = [
                MOVE (LREG r21, lrv);
                MOVE (LREG r22, rrv);
                MALLOC (LREG r29, INT 1);
                JMPNEQ (ADDR (CADDR nl1), REFREG (r21, 0), REFREG (r22, 0));
                MOVE (LREG ax, BOOL true);
                JUMP (ADDR (CADDR nl2));
                LABEL nl1;
                MOVE (LREG ax, BOOL false);
                LABEL nl2;
                MOVE (LREFREG (r29, 0), REG ax);
                MOVE (LREG ax, REG r29);
              ] in ([LABEL ls] @ plco @ prco @ pco, REFREG (bp, SLoc.get sloc))
            | E_NEQ -> (*<> 연산*)
              let nl1 = labelNew () in
              let nl2 = labelNew () in
              let _ = SLoc.add sloc in
              let pco = [
                MOVE (LREG r21, lrv);
                MOVE (LREG r22, rrv);
                MALLOC (LREG r29, INT 1);
                JMPNEQ (ADDR (CADDR nl1), REFREG (r21, 0), REFREG (r22, 0));
                MOVE (LREG ax, BOOL false);
                JUMP (ADDR (CADDR nl2));
                LABEL nl1;
                MOVE (LREG ax, BOOL true);
                LABEL nl2;
                MOVE (LREFREG (r29, 0), REG ax);
                MOVE (LREG ax, REG r29);
              ] in ([LABEL ls] @ plco @ prco @ pco, REFREG (bp, SLoc.get sloc))
          )
        | e1, e2 -> ( (*함수 app*)
          let fco, frv = exp2code en (labelNew ()) e1 in (*왼쪽을 코드로 만든다. fco에는 함수 전체의 코드, frv에는 함수 포인터가 들어가있음.*)
          let rco, rrv = exp2code en (labelNew ()) e2 in (*오른쪽을 코드로 만든다. rco에는 적용되는 값 전체를 만드는 코드, rrv에는 그 값의 위치가 들어가있음.*)
          let _ = SLoc.add sloc in
          let aco = [ (*두 값을 받아서 함수를 실행하고, 그 결과를 저장한다.*)
            MOVE (LREG r29, frv);
            MOVE (LREG r28, REFREG (r29, 1)); (*free variable의 위치를 잠시 r28로 옮긴다. CALL한 직후, 다시 꺼낼거임.*)
            MOVE (LREG r27, rrv); (*함수의 유일한 인자를 잠시 r27로 옮긴다. CALL한 직후, 다시 꺼낼거임.*)
            CALL (REFREG (r29, 0)); (*프로그램 카운터와 bp 위치를 동시에 push하고 frv로 점프한다.*)
            PUSH (REG ax);
          ] in ([LABEL ls] @ fco @ rco @ aco, REFREG (bp, SLoc.get sloc))
        )
  )
  | E_PAIR (EXPTY (e1, t1), EXPTY (e2, t2)) -> (*pair에 대해 다룬다.*) (*TODO: pair가 올바른 코드인지 확인하기*)
      let lco, lrv = exp2code en ls e1 in (*왼쪽을 코드로 만든다. 코드가 전부 실행되면 왼쪽의 결과가 포인터로서 lrv에 저장이 되게 된다.*)
      let rco, rrv = exp2code en ls e2 in (*오른쪽을 코드로 만든다. 코드가 전부 실행되면 오른쪽의 결과가 포인터로서 rrv에 저장이 되게 된다.*)
      let _ = SLoc.add sloc in
      let co = [
        MALLOC (LREG ax, INT 2); (*heap 공간 2개 만든다.*)
        MOVE (LREFREG (ax, 0), lrv); (*heap 공간 1번에 왼쪽 결과 포인터 저장한다.*)
        MOVE (LREFREG (ax, 1), rrv); (*heap 공간 2번에 오른쪽 결과 포인터 저장한다.*)
        PUSH (REG ax); (*heap을 추적하기 위해 stack에 해당 heap을 넣는다. stack은 sloc에 의해 추적되고 있음을 명심하자.*)
      ]
    in ([LABEL ls] @ lco @ rco @ co, REFREG (bp, SLoc.get sloc))
  | E_LET (de, EXPTY (e, t)) -> (*TODO: let이 올바른 코드인지 확인하기*)
      let co, en = dec2code en (labelNew ()) de
    in exp2code en ls e
  | _ -> (code0, UNIT)

(* dec2code : venv -> Mach.label -> Mono.dec -> Mach.code * venv *)
and dec2code (en: venv) ls de = match de with
  | D_VAL (PATTY (p, t1), EXPTY (e, t2)) ->
      let eco, erv = exp2code en (labelNew ()) e in
      let pco, pen = pat2code (labelNew ()) "when_error" (rvalue2loc erv) p
    in (eco @ pco, Dict.merge en pen)
  | D_REC (PATTY (p, t1), EXPTY (e, t2)) -> (
      match p with
        | P_VID (avid, VAR) -> (*다른 pat의 재귀는 불가능. 정확히는 함수 인자가 쪼개지면서 바깥 하나(첫번째 인자)만 rec이고 2번째부터는 rec이 아닌 그냥 일반 함수가 되어버림...*)
            let nen = Dict.insert (avid, rvalue2loc (REFREG (bp, SLoc.get sloc + 1))) en in
            let eco, erv = exp2code nen (labelNew ()) e in
            let pco, pen = pat2code (labelNew ()) "when_error" (rvalue2loc erv) p
          in (eco @ pco, Dict.merge en pen)
  )
  | D_DTYPE -> (code0, en)

(* mrule2code : venv -> Mach.label -> Mach.label -> Mono.mrule -> int -> Mach.code *)
and mrule2code (en: venv) (ls) (lf) (mr) (fvc) : Mach.code = match mr with
  | M_RULE (PATTY (p, t1), EXPTY (e, t2)) ->
    (*let lf2 = labelNew () in
    let lf3 = labelNew () in*)
    let csl = SLoc.get sloc in (*잉여 자원들을 전부 POP해버리기 위해서 스택의 차이를 측정하기 위해 일단 현재 위치를 저장한다.*)
    let pco, pen = pat2code (labelNew ()) lf(*lf2*) (L_DREF (L_REG bp, 0)) p in (*패턴 매칭하는 코드 만들고, 그로 인해 완성되는 환경을 돌려준다.*)
    let psc = (SLoc.get sloc) - csl in (*패턴 매칭 시에 의도치않게 JMP로 다음으로 넘어가버리는 경우를 고려해서 POP을 보정해준다!!! -> 지금은 r17로 하고 있음...*)
    let eco, rv = exp2code (Dict.merge en pen) (labelNew ()) e in (*기존 환경에다가 패턴 매칭으로 얻은 환경 더하고 그걸 가지고 표현식을 변환한다.*)
    let rec pgen n = (*pgen: pop generate. 함수 안에서 썼던 모든 값들은 함수 밖으로 나오면 무용지물이 됨. RETURN 이전에 모든 값을 다 POP 해버려야 RETURN이 올바르게 점프할 수 있음.*)
      if n > 0 then [POP (LREG r11)] @ pgen (n-1) else [] in (*r11을 쓰레기통마냥 사용해서 값들을 전부 POP할거임*)
    let rfc = (pgen ((SLoc.get sloc) - csl + fvc + 1)) @ [RETURN] in (*맨 처음에 클로저 PUSH + 함수의 유일한 인자 PUSH 한 것을 보정해줘야 되기 때문에 fvc개 만큼을 더한다.*)
    (*let pcc = [
      LABEL lf3;
      SUB (LREG r17, REG r17, INT 1);
      POP (LREG r11);
      JMPNEQ (ADDR (CADDR lf3), REG r17, INT 0);
      LABEL lf2;
      JMPNEQ (ADDR (CADDR lf3), REG r17, INT 0);
      JUMP (ADDR (CADDR lf));
    ] in*)
    (*패턴매칭 & 표현식을 전부 코드로 바꾼 결과를 저장하고 마무리한다. CALL와 RETURN이 bp를 변경함을 명심하자. 일단 함수를 CALL하기 전에 모든 환경 & 인자를 다 넘겨주고 시작하기 때문에 bp의 변경으로 인해 기존에 있었던 클로저 관련 문제가 생기지는 않는다.*)
    let _ = SLoc.set sloc csl
  in [LABEL ls] @ pco @ eco @ rfc(*@ pcc*)

(* mlist2inco : Mach.label -> env -> Mono.mlist -> int -> Mach.code *)
and mlist2inco (ls: Mach.label) (en: venv) (ml: Mono.mrule list) (fvc) : Mach.code =
  let _ = SLoc.set sloc ((SLoc.get sloc) + fvc + 1) in 
  let msc = [
    LABEL ls;
  ] in let rec pcl i = (*pcl: push closure. 함수의 시작점에는 자신이 가진 클로저를 스택에 넣어 관리해야 한다.*)
      if i < fvc then [PUSH (REFREG (r28, i))] @ pcl (i+1) (*클로저를 하나씩 PUSH한다. 통째로 넣으면 bp로 관리하기가 빡세다.*)
      else []
  in msc @ [PUSH (REG r27)] @ (pcl 0) @ mlist2code (labelNew ()) en ml fvc

(* mlist2code : Mach.label -> env -> Mono.mlist -> int -> Mach.code *)
and mlist2code (ls: Mach.label) (en: venv) (ml: Mono.mrule list) (fvc) : Mach.code =
  let nl = labelNew () in match ml with (*nl: next label. 다음 패턴 매칭으로 넘어가기 위한 새 레이블*)
  | [] -> [] (*만약 남은 패턴매칭이 없다면? 끝*)
  | mr :: rml -> (*rml: remaining mlist. 패턴매칭이 남았다면?*)
      let mrc =
        if (List.length rml)>0 then mrule2code en ls nl mr fvc (*다음 패턴매칭이 있다면, 현재 패턴매칭이 실패했을 때 넘어갈 수 있는 위치를 마련해준다*)
        else mrule2code en ls "when_error" mr fvc (*다음 패턴매칭이 없다면, 현재 패턴매칭을 실패했을 때 에러가 나야한다. *)
      in mrc @ (mlist2code nl en rml fvc) (*현재 패턴매칭 코드와 나머지 패턴 매칭 코드를 합쳐서 패턴매칭이 순서대로 코드상에 나오게 한다.*)

(* dl2code : dlist -> Mach.code * env *)
let rec dl2code dl ien = match dl with
  | [] -> ((*[LABEL "declaration"]*)code0, venv0) (*dlist가 비어있으면 빈 코드 반환*)
  | de :: dl' -> (*dlist에 dec가 있다면 코드로 추가*)
      let dec, deen = dec2code ien (labelNew ()) de in (*dec를 코드로 변환한다. 그 과정에서 환경도 가져온다.*)
      let dlc, dlen = dl2code dl' (Dict.merge ien deen) (*나머지 부분도 코드로 만들고 그 과정에서 환경을 가져온다.*)
    in (dec @ dlc, Dict.merge deen dlen) (*환경과 코드를 전부 합쳐서 반환한다.*)

let cofco = [ (*CONF 관리 코드. CONF는 함수 APP에서 관리되기 때문에 APP 과정에서 실행할 함수를 미리 정의해서 만들어두어야 한다.*)
  LABEL "conf_func"; (*원래 free variable이 있어야 하는 위치인 r28에는 CONF 이름이 온다. 마찬가지로 CONF의 유일한 값은 r27에 온다.*)
  MALLOC (LREG ax, INT 2);
  MOVE (LREFREG (ax, 1), REG r27); (*pat에서 다른 모든 데이터들은 0번째에 값이 존재하기 때문에 이거를 맞춰주지 않으면 문제가 생긴다!!!*)
  MOVE (LREFREG (ax, 0), REG r28); (*TODO: 이걸 뒤집었기 때문에 문제가 발생할 수 있는지 검토해보기!!!*)
  RETURN; (*CONF 내부 위치에 변수를 써도 문제가 없을까? -> 이미 클로저로 다 관리가 되어 문제가 없을 것 같기는 하다. 클로저를 만드는 주체는 exp2code에 있기에 여기서 굳이 신경쓸 필요가 없음.*)
]

let ecpco = [ (*예외 발생 코드*)
  LABEL "when_error"; (*예외 발생하면 오는 곳*)
  EXCEPTION (*Mach 예외 발생*)
]

(* program2code : Mono.program -> Mach.code *)
let program2code p = match p with
  | (dl, EXPTY (e, t)) ->
      let dlc, dlen = dl2code dl venv0 in (*dlc: dlist code, dlen: dlist env. 각 선언 목록을 코드로 바꾸고 그것들을 일렬로 나열한다*)
      let eco, rv = exp2code dlen (labelNew ()) e in (*eco: exp code, rv: rvalue. 표현식을 코드로 바꾼다. 해당 코드를 실행했을 때의 결과를 rvalue로 반환한다.*)
      let hco = (
        match t with
          | T_INT
          | T_BOOL
          | T_UNIT -> [HALT (REFREG (ax, 0))] (*모든 값이 한번 heap으로 감싸져서 포인터로서 관리되기 때문(숫자 2를 예로 들면, 포인터를 만듦으로서 2를 가리킬 것으로 기대되는 포인터를 관리한다. 실제로 아직은 2가 아니라 함수이더라도 언젠가는 2가 될 것이다??????)에 마지막에 딱 1번 그 포인터를 벗겨야 한다.*)
          | _ -> [HALT (REG ax)]
      )
    in cofco @ ecpco @ [LABEL start_label] @ dlc @ eco @ hco (*CONF 관리 코드 -> 예외 발생 코드 -> 시작점 -> 변수 정의하는 코드(맨 앞에 넣어서 코드가 bound되게 한다) -> 메인 코드 & 정지 코드*)
(*값을 포인터로 감싸는 건 ㅈ같은 생각이었던 것 같다. 이 자식 때문에 안생겨도 될 문제가 몇개나 생겼는지 모르겠다.*)
