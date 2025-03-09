open Common

exception NotImplemented

exception IllegalFormat

module Integer : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1

  let (++) x y = x + y
  let ( ** ) x y = x * y
  let (==) x y = x = y 
end

(* Problem 1-1 *)
(* Scalars *)

module Boolean : SCALAR with type t = bool 
=
struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true

  let (++) x y = x || y
  let ( ** ) x y = x && y
  let (==) x y = x = y
end

(* Problem 1-2 *)
(* Vectors *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = elem list

  exception VectorIllegal

  let create l = match l with
    | [] -> raise VectorIllegal
    | _ -> l
  let rec to_list v = match v with
    | [] -> []
    | x :: v' -> x :: to_list v'
  let rec dim (v: t) = match v with
    | [] -> 0
    | _ :: v' -> 1 + dim v'
  let rec nth v n = match v with
    | [] -> raise VectorIllegal
    | x :: v' ->
        if n=0 then x
        else nth v' (n-1)
  let rec (++) v1 v2 = match v1 with
    | [] ->
        if (dim v2)=0 then []
        else raise VectorIllegal
    | x :: v1' ->
        match v2 with
          | [] -> raise VectorIllegal
          | y :: v2' -> Scal.(x ++ y) :: (v1' ++ v2')
  let rec (==) v1 v2 = match v1 with
    | [] ->
        if (dim v2)=0 then true
        else raise VectorIllegal
    | x :: v1' ->
        match v2 with
          | [] -> raise VectorIllegal
          | y :: v2' ->
              if Scal.(x == y) then v1'==v2'
              else false
  let rec innerp v1 v2 = match v1 with
    | [] ->
        if (dim v2)=0 then Scal.zero
        else raise VectorIllegal
    | x :: v1' ->
        match v2 with
          | [] -> raise VectorIllegal
          | y :: v2' -> Scal.((Scal.(x ** y)) ++ (innerp v1' v2'))

end

(* Problem 1-3 *)
(* Matrices *)

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t
=
struct
  module Vec = VectorFn(Scal)

  type elem = Scal.t
  type t = Vec.t list

  exception MatrixIllegal

  let create l = match l with
    | [] -> raise MatrixIllegal
    | x :: _ ->
        let rn = Vec.dim (Vec.create x) in
        let rec create_rec lr d n =
          match lr with
            | [] ->
                if n=0 then []
                else raise MatrixIllegal
            | xr :: lr' ->
                let vxr = Vec.create xr in
                  if (Vec.dim vxr)=d then vxr :: create_rec lr' d (n-1)
                  else raise MatrixIllegal
        in create_rec l rn rn
  let identity n =
    if n<0 then raise MatrixIllegal
    else
      let rec cz k l =
        if k>0 then Scal.zero :: cz (k-1) l
        else l
      in
      let rec iden_rec k n =
        if k<n then Vec.create (cz k (Scal.one :: cz (n-k-1) [])) :: iden_rec (k+1) n
        else []
      in iden_rec 0 n
  let dim m = match m with
    | [] -> raise MatrixIllegal
    | x :: _ -> Vec.dim x
  let transpose (m: Vec.t list) =
    let rec cfr mh = match mh with
      | [] -> []
      | l :: mh' -> match (Vec.to_list l) with
        | [] -> []
        | x :: _ -> x :: cfr mh'
    in
    let rec crr mh = match mh with
      | [] -> []
      | l :: mh' -> match (Vec.to_list l) with
        | [] -> []
        | _ :: l' -> (Vec.create l') :: crr mh'
    in
    let rec tran_rec mh =
      Vec.create (cfr mh) :: tran_rec (crr mh)
    in tran_rec m
  let rec to_list m = match m with
    | [] -> []
    | l :: m' -> (Vec.to_list l) :: to_list m'
  let get m r c =
    let d = dim m in
    if r>=d || c>=d || r<0 || c<0 then raise MatrixIllegal
    else
      let rec get_row m r c = match m with
        | [] -> raise MatrixIllegal (* It can be fixed *)
        | l :: m' ->
            if r=0 then Vec.nth l c
            else get_row m' (r-1) c
      in get_row m r c
  let (++) m1 m2 =
    if dim m1 != dim m2 then raise MatrixIllegal
    else
      let rec add_rec m1 m2 = match m1 with
        | [] -> []
        | x :: m1' -> match m2 with
          | [] -> raise MatrixIllegal (* It can be fixed *)
          | y :: m2' -> Vec.(x ++ y) :: add_rec m1' m2'
      in add_rec m1 m2
  let ( ** ) m1 m2 =
    if dim m1 != dim m2 then raise MatrixIllegal
    else
      raise MatrixIllegal (* 이거 다시 만들기. 잘못 만들었음... *)
      (* in mul_rec (transpose m1) m2*)
  let (==) m1 m2 =
    if dim m1 != dim m2 then raise MatrixIllegal
    else
      let rec eq_rec m1 m2 = match m1 with
        | [] -> true
        | x :: m1' -> match m2 with
          | [] -> false
          | y :: m2' -> Vec.(x == y) || eq_rec m1' m2'
      in eq_rec m1 m2
end

(* Problem 2-1 *)
(* Closure *)

module ClosureFn (Mat : MATRIX) :
sig
  val closure : Mat.t -> Mat.t
end
=
struct
  let closure _ = raise NotImplemented
end

(* Problem 2-2 *)
(* Applications to Graph Problems *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach _ = raise NotImplemented

let al = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  false; false];
   [false; true;  true;  false; true;  false];
   [false; true;  false; true;  true;  true];
   [false; false; true;  true;  true;  false];
   [false; false; false; true;  false; true]]

let solution_al' = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true]]

module Distance : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)

  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let distance _ = raise NotImplemented

let dl =
  [[  0;  -1;  -1;  -1;  -1;  -1 ];
   [ -1; 0  ; 35 ; 200; -1 ; -1  ];
   [ -1; 50 ; 0  ; -1 ; 150; -1  ];
   [ -1; 75;  -1 ; 0  ; 100; 25  ];
   [ -1; -1 ; 50 ; 65 ; 0  ; -1  ];
   [ -1; -1 ; -1 ; -1 ; -1 ; 0   ]]

let solution_dl' =
  [[0;  -1;  -1;  -1;  -1;  -1  ];
   [-1; 0;   35;  200; 185; 225 ];
   [-1; 50;  0;   215; 150; 240 ];
   [-1; 75;  110; 0;   100; 25  ];
   [-1; 100; 50;  65;  0;   90  ];
   [-1; -1;  -1;  -1;  -1;  0   ]]

module Weight : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)
 
  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let weight _ = raise NotImplemented

let ml =
  [[-1; 0  ; 0  ; 0  ; 0  ; 0   ];
   [0 ; -1 ; 10 ; 100; 0  ; 0   ];
   [0 ; 50 ; -1 ; 0  ; 150; 0   ];
   [0 ; 75 ; 0  ; -1 ; 125; 40 ];
   [0 ; 0  ; 25 ; -1 ; -1 ; 0   ];
   [0 ; 0  ; 0  ; 0  ; 0  ; -1  ]]

let solution_ml' =
  [[-1; 0;  0;   0;   0;   0  ];
   [0;  -1; 25;  100; 100; 40 ];
   [0;  75; -1;  150; 150; 40 ];
   [0;  75; 25;  -1;  125; 40 ];
   [0;  75; 25;  -1;  -1;  40 ];
   [0;  0;  0;   0;   0;   -1 ]]

let _ =
  try 
  if reach al = solution_al' && distance dl = solution_dl' && weight ml = solution_ml' then
    print_endline "\nYour program seems fine (but no guarantee)!"
  else
    print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!" 

