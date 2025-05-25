open Fjava

exception NotImplemented
exception TypeError
exception Stuck

let rec lookup k d = match d with
  | [] -> raise TypeError
  | (k', v') :: d' -> if k = k' then v' else lookup k d'
let delete k d = List.filter (fun (k', _) -> k <> k') d
let insert (k, v) d = (k, v)::(delete k d)
let filter f d = List.filter (fun (k, v) -> f (k, v)) d
let merge old_dict new_dict = List.fold_left (fun old (k, v) -> insert (k, v) old) old_dict new_dict
let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x, y) :: zip xs ys
  | _ -> raise Stuck
let flip (x, y) = (y, x)

(*mlookup: auxilirary function that return method by name*)
let rec mlookup mname mdecl = match mdecl with
  | [] -> raise Stuck
  | mdec :: mdecl' -> let (_, x, _, _) = mdec in
    if mname=x then mdec else mlookup mname mdecl'

(*clookup: auxilirary function that return class by name*)
let rec clookup cname cdecl = match cdecl with
  | [] -> raise TypeError
  | cdec :: cdecl' -> let (x, _, _, _, _) = cdec in
    if cname=x then cdec else clookup cname cdecl'

(*typeOf: check if certain method name exists*)
let rec mexists mdecl mname = match mdecl with
  | [] -> false
  | (_, mname', _, _) :: mdecl' ->
      if mname=mname' then true else mexists mdecl' mname

(*manem2mbody: get method body by name. find it from a cdecl*)
let rec mname2mbody cdecl cname mname =
  let _, sname, _, _, mdecl = try clookup cname cdecl with TypeError -> raise Stuck
  in if mexists mdecl mname then
        let _, _, params, mbody' = mlookup mname mdecl
      in (List.map snd params, mbody')
    else mname2mbody cdecl sname mname

(*istyeq: check if two list of types are equal*)
let rec istyeq l1 l2 = match (l1, l2) with
  | ([], []) -> true
  | (x1 :: l1', x2 :: l2') -> x1=x2 && istyeq l1' l2'
  | _ -> false

(*isleq: check if two list has same length*)
let isleq l1 l2 = List.length l1 = List.length l2

(*getindex: get index of element in a certain list*)
let getindex x l =
  let rec aux i = function
    | [] -> raise Stuck
    | d :: l' -> if x=d then i else aux (i + 1) l'
  in aux 0 l

(*class2field: get all fields from a certain class*)
let rec class2field cdecl cname =
  if cname="Object" then []
  else let _, sname, field, _, _ = clookup cname cdecl
    in field @ class2field cdecl sname

(*issup: check if type s2 is super type of type s1*)
let rec issup cdecl s1 s2 =
  if s1=s2 then true
  else if s1="Object" then false
  else let _, sname, _, _, _ = clookup s1 cdecl in
    issup cdecl sname s2

(*issupl: check if list of type l2 is super type of list of type l1*)
let rec issupl cdecl l1 l2 = match (l1, l2) with
  | ([], []) -> true
  | (x1 :: l1', x2 :: l2') -> issup cdecl x1 x2 && issupl cdecl l1' l2'
  | _ -> false

(*method2type: auxiliary function that get type of method by name*)
let rec method2type cdecl mname cname =
  let _, sname, _, _, mdecl = clookup cname cdecl in
  try
      let mret, mname, mparams, _ = mlookup mname mdecl
    in (List.map fst mparams, mret)
  with Stuck -> method2type cdecl mname sname

(*override: auxiliary function that check if override is ok*)
let override cdecl mname sname params cret =
  try
    let mparams, mret = method2type cdecl mname sname in
    istyeq (params @ [cret]) (mparams @ [mret])
  with TypeError -> true

(*exp2type: auxiliary function that get type of exp by using context*)
let rec exp2type cdecl env exp = match exp with
  | Var v -> lookup v env
  | Field (exp, f) ->
      let c0 = exp2type cdecl env exp in
      let cfield = class2field cdecl c0 in
      let rec find field = match field with
        | [] -> raise TypeError
        | (ty, x) :: field' -> if f = x then ty else find field'
    in find cfield
  | Method (exp, mname, expl) ->
      let c0 = exp2type cdecl env exp in
      let mparams, cname = method2type cdecl mname c0 in
      let etypel = List.map (fun e -> exp2type cdecl env e) expl
    in if issupl cdecl etypel mparams then cname else raise TypeError
  | New (ty, expl) ->
      let etypel = List.map (fun e -> exp2type cdecl env e) expl in
      let ftypel = List.map fst (class2field cdecl ty)
    in if issupl cdecl etypel ftypel then ty else raise TypeError
  | Cast (newty, exp) ->
      let ty' = exp2type cdecl env exp in
      let _ = if (issup cdecl newty ty') || (issup cdecl ty' newty) then () else print_string "Stupid Warning\n"
    in newty

(*isconok: check if constructor is ok*)
let isconok cdecl cname =
    let _, sname, field, constructor, _ = clookup cname cdecl in
    let cname', params, sparams, assigns = constructor in
    let sfield = class2field cdecl sname in
    let _ = if cname=cname' then () else raise TypeError in
    let _ = if (isleq sfield sparams) && (isleq field assigns) then () else raise TypeError in
    let _ = if istyeq params (sfield @ field) then () else raise TypeError in
    let _ = if List.for_all (fun (x1, x2) -> x1 = x2) assigns then () else raise TypeError in
    let ptypel = List.map snd params in
    let atypel = List.map snd assigns
  in istyeq ptypel (sparams @ atypel)

(*ismok: check if method list is ok*)
let rec ismok cdecl cname =
  let _, sname, _, _, mdecl = clookup cname cdecl in
  let rec ismok_aux cname sname mdecl = (
    match mdecl with
    | [] -> true
    | mdec :: mdecl' -> let c0, mname, params, mbody = mdec in
        let etype = exp2type cdecl (merge [("this", cname)] (List.map flip params)) mbody
      in (issup cdecl etype c0) && (override cdecl mname sname (List.map fst params) c0) && (ismok_aux cname sname mdecl')
  ) in ismok_aux cname sname mdecl

(*iscok: check if list of class is ok*)
let iscok cdecl =
  let rec iscok_rec cdecl' = (
    match cdecl' with
      | [] -> true
      | cdec :: remain_cdecl -> let cname, _, _, _, _ = cdec in
        (isconok cdecl cname) && (ismok cdecl cname) && (iscok_rec remain_cdecl)
  ) in iscok_rec cdecl

(*typeOf: get a type of an exp of a program*)
let typeOf program = let cdecl, exp = program
  in if iscok cdecl then exp2type cdecl [] exp
    else raise TypeError

(*isredex: check if a exp is redex. if so, return true. if not, return false*)
let rec isredex exp = match exp with
  | Var v -> false
  | New (_, el) -> List.exists (fun x -> isredex x) el
  | _ -> true

(*firstredex: find the first redex from a list of exp. if there is no such redex, raise Stuck*)
let rec firstredex expl = match expl with
  | [] -> raise Stuck
  | e :: expl' -> if isredex e then 0 else (firstredex expl') + 1

(*sub: substitute exp from another by using environment*)
let rec sub env exp = match exp with
  | Var v -> lookup v env
  | Field (exp, f) -> Field (sub env exp, f)
  | Method (exp, mname, expl) -> Method (sub env exp, mname, subl env expl)
  | New (ty, expl) -> New (ty, subl env expl)
  | Cast (ty, exp) -> Cast (ty, sub env exp)
and subl env expl = List.map (fun x -> sub env x) expl

(*trystep: try a step by using Some and None.*)
let rec trystep cdecl exp =
  try Some (stepexp cdecl exp) with Stuck -> None
(*stepi: step ith exp in the list*)
and stepi cdecl expl i =
  let rec stepi_aux li idx = match li with
    | [] -> []
    | e :: li' -> (if idx=i then stepexp cdecl e else e) :: (stepi_aux li' (idx + 1))
  in stepi_aux expl 0
(*stepexp: step exp once*)
and stepexp cdecl exp = match exp with
  | Field (exp, f) -> ( match exp with
      | New (ty, expl) -> (
        match trystep cdecl exp with
          | Some x -> Field (x, f)
          | None -> let field = List.map snd (class2field cdecl ty)
            in List.nth expl (getindex f field)
      )
      | _ -> Field (stepexp cdecl exp, f)
  )
  | Method (exp, mname, expl) -> ( match exp with
    | New (ty, expl') -> ( match trystep cdecl exp with
      | Some x -> Method (x, mname, expl)
      | None -> try Method (exp, mname, stepi cdecl expl (firstredex expl))
          with Stuck -> let params, e0 = mname2mbody cdecl ty mname
        in sub (merge [("this", exp)] (zip params expl)) e0
    )
    | _ -> Method (stepexp cdecl exp, mname, expl)
  )
  | New (ty, expl) -> New (ty, stepi cdecl expl (firstredex expl))
  | Cast (ty, exp) -> ( match exp with
    | New (ty', expl) -> ( match trystep cdecl exp with
      | Some x -> Cast (ty, x)
      | None -> if issup cdecl ty' ty then exp else raise Stuck
    )
    | _ -> Cast (ty, stepexp cdecl exp)
  )
  | _ -> raise Stuck

(*step: step a program. the most work is done in the function stepexp.*)
let step program =
  let cdecl, exp = program
  in (cdecl, stepexp cdecl exp)

let typeOpt p = try Some (typeOf p) with TypeError -> None
let stepOpt p = try Some (step p) with Stuck -> None
let rec multiStep p = try multiStep (step p) with Stuck -> p

let rec stepStream exp =
  let rec steps exp = match stepOpt exp with
    | None -> Stream.from (fun _ -> None)
    | Some exp' -> Stream.icons exp' (steps exp')
  in Stream.icons exp (steps exp)
