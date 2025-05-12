exception MonomorphiseError
exception NotImplemented

(*****************)
(* Aux functions *)
(*****************)
let rec avspat v = match v with 
    Core.P_WILD -> Set_type.singleton "_"
  | Core.P_INT av -> Set_type.singleton (string_of_int av)
  | Core.P_UNIT -> Set_type.singleton "()"
  | Core.P_VID (av, Core.VAR) -> Set_type.singleton av
  | Core.P_VID (av, Core.CON) -> Set_type.singleton av
  | Core.P_VIDP (_, pt) -> avspatty pt
  | Core.P_PAIR (pt, pt') -> Set_type.union (avspatty pt) (avspatty pt')
  | _ -> Set_type.empty
and avspatty (Core.PATTY (p, ty)) = avspat p

let avsdec d = match d with
    Core.D_VAL (pt, _) -> avspatty pt
  | Core.D_REC (pt, _) -> avspatty pt
  | _ -> Set_type.empty

let rec tsexp av e = match e with 
    Core.E_FUN ml -> List.fold_left (fun s' s -> Set_type.union s s') Set_type.empty (List.map (tsmrule av) ml)
  | Core.E_APP (et, et') -> Set_type.union (tsexpty av et) (tsexpty av et')
  | Core.E_PAIR (et, et') -> Set_type.union (tsexpty av et) (tsexpty av et')
  | Core.E_LET (d, et) -> Set_type.union (tsdec av d) (if Set_type.mem av (avsdec d) then Set_type.empty else tsexpty av et)
  | _ -> Set_type.empty
and tsexpty av e = match e with 
    Core.EXPTY (Core.E_VID (av', Core.VAR), ty) -> if av = av' then Set_type.singleton ty else Set_type.empty
  | Core.EXPTY (e, ty) -> tsexp av e

and tsmrule av (Core.M_RULE (pt, et)) = if Set_type.mem av (avspatty pt) then Set_type.empty else tsexpty av et

and tsdec av d = match d with 
    Core.D_VAL (pt, et) -> tsexpty av et
  | Core.D_REC (pt, et) -> if Set_type.mem av (avspatty pt) then Set_type.empty else tsexpty av et
  | _ -> Set_type.empty

let rec tsprogram av p = match p with 
    ([], et) -> tsexpty av et
  | (d :: dl, et) -> Set_type.union (tsdec av d) (if Set_type.mem av (avsdec d) then Set_type.empty else tsprogram av (dl, et))

let rec tvsty t = match t with 
    Core.T_PAIR (ty, ty') -> Set_type.union (tvsty ty) (tvsty ty')
  | Core.T_FUN (ty, ty') -> Set_type.union (tvsty ty) (tvsty ty')
  | Core.T_VAR tv -> Set_type.singleton tv
  | _ -> Set_type.empty

let rec tpat av p = match p with
    Core.P_VIDP (_, pt) -> tpatty av pt
  | Core.P_PAIR (pt, pt') -> (try tpatty av pt with MonomorphiseError -> tpatty av pt')
  | _ -> raise MonomorphiseError

and tpatty av p = match p with
    Core.PATTY (Core.P_WILD, ty) -> if av = "_" then ty else raise MonomorphiseError
  | Core.PATTY (Core.P_INT av', ty) -> if av' = int_of_string av then ty else raise MonomorphiseError
  | Core.PATTY (Core.P_UNIT, ty) -> if av = "()" then ty else raise MonomorphiseError
  | Core.PATTY (Core.P_VID (av', Core.VAR), ty) -> if av = av' then ty else raise MonomorphiseError
  | Core.PATTY (Core.P_VID (av', Core.CON), ty) -> if av = av' then ty else raise MonomorphiseError
  | Core.PATTY (p, ty) -> tpat av p

let tdec av d = match d with 
    Core.D_VAL (pt, _) -> tpatty av pt
  | Core.D_REC (pt, _) -> tpatty av pt
  | _ -> raise MonomorphiseError

let rec sexp ((av, _, _) as sub) e = match e with 
    Core.E_FUN ml -> Core.E_FUN (List.map (smrule sub) ml)
  | Core.E_APP (et, et') -> Core.E_APP (sexpty sub et, sexpty sub et')
  | Core.E_PAIR (et, et') -> Core.E_PAIR (sexpty sub et, sexpty sub et')
  | Core.E_LET (d, et) -> Core.E_LET (sdec sub d, if Set_type.mem av (avsdec d) then et else sexpty sub et)
  | e -> e
and sexpty ((av, ty, av') as sub) p = match p with 
    (Core.EXPTY (Core.E_VID (av'', Core.VAR), ty')) as et -> if av = av'' && ty = ty' then Core.EXPTY (Core.E_VID (av', Core.VAR), ty') else et
  | Core.EXPTY (e, ty) -> Core.EXPTY (sexp sub e, ty)

and smrule ((av, _, _) as sub) (Core.M_RULE (pt, et)) = Core.M_RULE (pt, if Set_type.mem av (avspatty pt) then et else sexpty sub et)

and sdec ((av, _, _) as sub) d = match d with
    Core.D_VAL (pt, et) -> Core.D_VAL (pt, sexpty sub et)
  | Core.D_REC (pt, et) -> Core.D_REC (pt, if Set_type.mem av (avspatty pt) then et else sexpty sub et)
  | Core.D_DTYPE -> Core.D_DTYPE

let rec sprogram ((av, _, _) as sub) p = match p with 
    ([], et) -> ([], sexpty sub et)
  | (d :: dl, et) ->
     let d' = sdec sub d in
     let (dl', et') = if Set_type.mem av (avsdec d) then (dl, et) else sprogram sub (dl, et) in
     (d' :: dl', et')

let rec epat ((av, av') as sub) p = match p with
    Core.P_VID (av'', Core.VAR) -> if av = av'' then Core.P_VID (av', Core.VAR) else Core.P_WILD
  | Core.P_VIDP (v, pt) -> Core.P_VIDP (v, epatty sub pt)
  | Core.P_PAIR (pt, pt') -> Core.P_PAIR (epatty sub pt, epatty sub pt')
  | p -> p
and epatty sub (Core.PATTY (p, ty)) = Core.PATTY (epat sub p, ty)

let rec eexp ((av, av') as sub) e = match e with 
    (Core.E_VID (av'', Core.VAR)) as e -> if av = av'' then Core.E_VID (av', Core.VAR) else e
  | Core.E_FUN mrl -> Core.E_FUN (List.map (emrule sub) mrl)
  | Core.E_APP (et, et') -> Core.E_APP (eexpty sub et, eexpty sub et')
  | Core.E_PAIR (et, et') -> Core.E_PAIR (eexpty sub et, eexpty sub et')
  | Core.E_LET (d, et) -> Core.E_LET (edec sub d, if Set_type.mem av (avsdec d) then et else eexpty sub et)
  | e -> e
and eexpty sub (Core.EXPTY (e, ty)) = Core.EXPTY (eexp sub e, ty)

and emrule ((av, _) as sub) (Core.M_RULE (pt, et)) = Core.M_RULE (pt, if Set_type.mem av (avspatty pt) then et else eexpty sub et)

and edec ((av, _) as sub) d = match d with
    Core.D_VAL (pt, et) -> Core.D_VAL (pt, eexpty sub et)
  | Core.D_REC (pt, et) -> Core.D_REC (pt, if Set_type.mem av (avspatty pt) then et else eexpty sub et)
  | Core.D_DTYPE -> Core.D_DTYPE

let rec build t1 t2 = match (t1, t2) with 
    (Core.T_INT, Core.T_INT) -> Dict.empty
  | (Core.T_BOOL, Core.T_BOOL) -> Dict.empty
  | (Core.T_UNIT, Core.T_UNIT) -> Dict.empty
  | (Core.T_NAME tn, Core.T_NAME tn') -> if tn = tn' then Dict.empty else raise MonomorphiseError
  | (Core.T_PAIR (ty1, ty2), Core.T_PAIR (ty'1, ty'2)) -> Dict.merge (build ty1 ty'1) (build ty2 ty'2)
  | (Core.T_FUN (ty1, ty2), Core.T_FUN (ty'1, ty'2)) -> Dict.merge (build ty1 ty'1) (build ty2 ty'2)
  | (Core.T_VAR tv, ty) -> Dict.singleton (tv, ty)
  | (_, _) -> raise MonomorphiseError

let tag av num = av ^ "_" ^ (string_of_int num)

let removetag av = 
  let idx = try String.index av '_' with _ -> -1 in
  if idx > -1 then String.sub av 0 idx
  else av

(********************)
(* Check type equal *)
(********************)
let checkeqvid (base_av, base_is) (av, is) = 
  (base_is = is) && (base_av = av)

let rec checkeqty base t = match (t, base) with 
    (Core.T_INT, Core.T_INT) -> true
  | (Core.T_BOOL, Core.T_BOOL) -> true
  | (Core.T_UNIT, Core.T_UNIT) -> true
  | (Core.T_NAME tn, Core.T_NAME base_tn) -> base_tn = tn
  | (Core.T_PAIR (ty, ty'), Core.T_PAIR (base_ty, base_ty')) -> (checkeqty base_ty ty) && (checkeqty base_ty' ty')
  | (Core.T_FUN (ty, ty'), Core.T_FUN (base_ty, base_ty')) -> (checkeqty base_ty ty) && (checkeqty base_ty' ty')
  | (_, _) -> false

let rec checkeqpat base p = match (p, base) with
    (Core.P_WILD, _) -> true
  | (Core.P_INT n, Core.P_INT base_n) -> base_n = n
  | (Core.P_BOOL b, Core.P_BOOL base_b) -> base_b = b
  | (Core.P_UNIT, Core.P_UNIT) -> true
  | (Core.P_VID v, Core.P_VID base_v) -> checkeqvid base_v v
  | (Core.P_VIDP (v, pt), Core.P_VIDP (base_v, base_pt)) -> (checkeqvid base_v v) && (checkeqpatty base_pt pt)
  | (Core.P_PAIR (pt, pt'), Core.P_PAIR (base_pt, base_pt'))-> (checkeqpatty base_pt pt) && (checkeqpatty base_pt' pt')
  | (_, _) -> false
and checkeqpatty (Core.PATTY (base_p, base_ty)) (Core.PATTY (p, ty)) = (checkeqpat base_p p) && (checkeqty base_ty ty)

let rec checkeq base d = 
  try 
    match (d, base) with
      (Core.D_VAL (pt, _), Core.D_VAL (base_pt, _)) -> checkeqpatty base_pt pt
    | (Core.D_REC (pt, _), _) -> false
    | (Core.D_DTYPE, _) -> false
    | (_, _) -> false
  with _ -> false
              
(*************)
(* Merge dec *)
(*************)
let rec mergepat base p = match (p, base) with 
    (Core.P_WILD, _)  -> base
  | (Core.P_VID v, _) -> p
  | (Core.P_VIDP (v, pt), Core.P_VIDP (_, base_pt)) -> Core.P_VIDP (v, mergepatty base_pt pt)
  | (Core.P_PAIR (pt, pt'), Core.P_PAIR (base_pt, base_pt')) -> Core.P_PAIR (mergepatty base_pt pt, mergepatty base_pt' pt')
  | (_, _) -> p
and mergepatty (Core.PATTY (base_p, _)) (Core.PATTY (p, et)) = Core.PATTY (mergepat base_p p, et)

let mergedec base dl = 
  let mergebase base d = match (d, base) with
      (Core.D_VAL (pt, et), Core.D_VAL (base_pt, _)) -> Core.D_VAL (mergepatty base_pt pt, et)
    | (Core.D_REC (pt, et), Core.D_REC (base_pt, _)) -> Core.D_REC (mergepatty base_pt pt, et)
    | (Core.D_DTYPE, Core.D_DTYPE) -> d
    | (_, _) -> raise MonomorphiseError
  in
  let base = List.fold_left mergebase base dl in 
  let (dl'', _) = List.fold_left (fun (dl', first) d -> if checkeq base d 
                                                        then (if first then (dl' @ [base], false) else (dl', first))
                                                        else (dl' @ [d], first)) ([], true) dl in
  dl''

(***************************)
(* Check exist polymophism *)
(***************************)
let rec checkty t = match t with 
    Core.T_VAR _ -> true
  | Core.T_PAIR (ty, ty') -> (checkty ty) || (checkty ty')
  | Core.T_FUN (ty, ty') -> (checkty ty) || (checkty ty')
  | _ -> false

let rec checkpat p = match p with
    Core.P_VIDP (_, pt) -> checkpatty pt
  | Core.P_PAIR (pt, pt') -> (checkpatty pt) || (checkpatty pt')
  | _ -> false
and checkpatty (Core.PATTY (p, ty)) = (checkpat p) || (checkty ty)

let rec checkexp e = match e with
    Core.E_FUN ml -> List.fold_left checkmrule false ml
  | Core.E_APP (et, et') -> (checkexpty et) || (checkexpty et')
  | Core.E_PAIR (et, et') -> (checkexpty et) || (checkexpty et')
  | Core.E_LET (d, et) -> (checkdec false d) || (checkexpty et)
  | _ -> false
and checkexpty (Core.EXPTY (e, ty)) = (checkexp e) || (checkty ty)

and checkmrule ispoly (Core.M_RULE (pt, et)) = (checkpatty pt) || (checkexpty et) || ispoly

and checkdec ispoly d = match d with 
    Core.D_VAL (pt, et) -> (checkpatty pt) || (checkexpty et) || ispoly
  | Core.D_REC (pt, et) -> (checkpatty pt) || (checkexpty et) || ispoly
  | Core.D_DTYPE -> false || ispoly

let checkpoly (dl, et) = (List.fold_left checkdec false dl) || (checkexpty et)

                                                                 
(*************************************************)
(* Pass 1 : Eliminate monomorphic type variables *)
(*************************************************)
let rec mty ptvs ty = match ty with
    (Core.T_VAR tv) as ty -> if Set_type.mem tv ptvs then ty else Core.T_UNIT
  | Core.T_PAIR (ty, ty') -> Core.T_PAIR (mty ptvs ty, mty ptvs ty')
  | Core.T_FUN (ty, ty') -> Core.T_FUN (mty ptvs ty, mty ptvs ty')
  | ty -> ty

let rec mpat ptvs p = match p with 
    Core.P_VIDP (vid, pt) -> Core.P_VIDP (vid, mpatty ptvs pt)
  | Core.P_PAIR (pt, pt') -> Core.P_PAIR (mpatty ptvs pt, mpatty ptvs pt')
  | p -> p
and mpatty ptvs (Core.PATTY (p, ty)) = Core.PATTY (mpat ptvs p, mty ptvs ty)

let rec mexp mtvs ptvs e = match e with 
    Core.E_FUN mrl -> Core.E_FUN (List.map (mmrule mtvs ptvs) mrl)
  | Core.E_APP (et, et') -> Core.E_APP (mexpty mtvs ptvs et, mexpty mtvs ptvs et')
  | Core.E_PAIR (et, et') -> Core.E_PAIR (mexpty mtvs ptvs et, mexpty mtvs ptvs et')
  | Core.E_LET (d, et) -> Core.E_LET (mdec mtvs ptvs d, mexpty mtvs ptvs et)
  | e -> e
and mexpty mtvs ptvs (Core.EXPTY (e, ty)) = Core.EXPTY (mexp mtvs ptvs e, mty ptvs ty)

and mmrule mtvs ptvs (Core.M_RULE ((Core.PATTY (_, ty)) as pt, et)) = 
  let mtvs' = Set_type.union mtvs (Set_type.diff (tvsty ty) ptvs) in
  Core.M_RULE (mpatty ptvs pt, mexpty mtvs' ptvs et)

and mdec mtvs ptvs dl = match dl with
    Core.D_VAL ((Core.PATTY (_, ty)) as pt, et) ->
    let ptvs' = Set_type.union ptvs (Set_type.diff (tvsty ty) mtvs) in
    Core.D_VAL (mpatty ptvs' pt, mexpty mtvs ptvs' et)
  | Core.D_REC ((Core.PATTY (_, ty)) as pt, et) ->
     let ptvs' = Set_type.union ptvs (Set_type.diff (tvsty ty) mtvs) in
     Core.D_REC (mpatty ptvs' pt, mexpty mtvs ptvs' et)
  | d -> d

let mprogram (dl, et) = (List.map (mdec Set_type.empty Set_type.empty) dl, mexpty Set_type.empty Set_type.empty et)

(*************************************************)
(* Pass 2 : Eliminate polymorphic type variables *)
(*************************************************)
let rec pty im ty = match ty with 
    Core.T_PAIR (ty, ty') -> Core.T_PAIR (pty im ty, pty im ty')
  | Core.T_FUN (ty, ty') -> Core.T_FUN (pty im ty, pty im ty')
  | Core.T_VAR tv -> (match Dict.lookup tv im with Some t -> t | _ -> Core.T_UNIT)
  | ty -> ty

let rec ppat im p = match p with  
    Core.P_VIDP (v, pt) -> Core.P_VIDP (v, ppatty im pt)
  | Core.P_PAIR (pt, pt') -> Core.P_PAIR (ppatty im pt, ppatty im pt')
  | p -> p
and ppatty im (Core.PATTY (p, ty)) = Core.PATTY (ppat im p, pty im ty)

let rec pexp im e = match e with 
    Core.E_FUN ml -> Core.E_FUN (List.map (pmrule im) ml)
  | Core.E_APP (et, et') -> Core.E_APP (pexpty im et, pexpty im et')
  | Core.E_PAIR (et, et') -> Core.E_PAIR (pexpty im et, pexpty im et')
  | Core.E_LET (Core.D_DTYPE, et) -> Core.E_LET (Core.D_DTYPE, pexpty im et)
  | Core.E_LET (d, et) ->
     let et' = pexpty im et in
     let forav (dl, et) av =
       let forty (n, dl, et) ty =
         let ty' = tdec av d in
         let av' = tag av n in
         let im' = Dict.merge im (build ty' ty) in
         (n + 1, (pdec (av, av') im' d) :: dl, sexpty (av, ty, av') et) 
       in
       let (_, dl', et') = Set_type.fold forty (1, dl, et) (Set_type.add (mty Set_type.empty (tdec av d)) (tsexpty av et)) in
       (dl', et')
     in  
     let (dl'', et'') = Set_type.fold forav ([], et') (avsdec d) in
     let Core.EXPTY (e, _) = List.fold_left (fun (Core.EXPTY (_, ty) as et) d -> Core.EXPTY (Core.E_LET (d, et), ty)) et'' dl'' in
     e
  | e -> e
and pexpty im (Core.EXPTY (e, ty)) = Core.EXPTY (pexp im e, pty im ty)

and pmrule im (Core.M_RULE (pt, et)) = Core.M_RULE (ppatty im pt, pexpty im et)

and pdec sub im d = match d with 
    Core.D_VAL (pt, et) -> Core.D_VAL (ppatty im (epatty sub pt), pexpty im et)
  | Core.D_REC (pt, et) -> Core.D_REC (ppatty im (epatty sub pt), pexpty im (eexpty sub et))
  | Core.D_DTYPE -> Core.D_DTYPE

and pdec' im d = match d with 
    Core.D_VAL (pt, et) -> Core.D_VAL (ppatty im pt, pexpty im et)
  | Core.D_REC (pt, et) -> Core.D_REC (ppatty im pt, pexpty im et)
  | Core.D_DTYPE -> Core.D_DTYPE

let rec pprogram im p = match p with 
    ([], et) -> ([], pexpty im et)
  | (d :: dl, et) ->
     let program' = pprogram im (dl, et) in
     let forav (dl, program) av =
       let forty (n, dl, program) ty =
         let ty' = tdec av d in
         let av' = tag av n in
         let im' = Dict.merge im (build ty' ty) in
         (n + 1, (pdec (av, av') im' d) :: dl, sprogram (av, ty, av') program)
       in
       let (_, dl', program') = Set_type.fold forty (1, dl, program) (Set_type.add (mty Set_type.empty (tdec av d)) (tsprogram av program)) 
       in
       (dl', program')  
     in
     let dvar = avsdec d in
     let (dl'', (dl', et')) = Set_type.fold forav ([], program') dvar in
     let dl'' = mergedec d (List.rev dl'') in
     (dl'' @ dl', et')

(***********************)
(* Pass 3 : Conversion *)
(***********************)
let cvid v = match v with 
    (av, Core.VAR) -> (av, Mono.VAR)
  | (av, Core.CON) -> (av, Mono.CON)
  | (av, Core.CONF) -> (av, Mono.CONF)

let rec cty t = match t with 
    Core.T_INT -> Mono.T_INT
  | Core.T_BOOL -> Mono.T_BOOL
  | Core.T_UNIT -> Mono.T_UNIT
  | Core.T_NAME tn -> Mono.T_NAME tn
  | Core.T_PAIR (ty, ty') -> Mono.T_PAIR (cty ty, cty ty')
  | Core.T_FUN (ty, ty') -> Mono.T_FUN (cty ty, cty ty')
  | _ -> raise MonomorphiseError

let rec cpat p = match p with
    Core.P_WILD -> Mono.P_WILD
  | Core.P_INT n -> Mono.P_INT n
  | Core.P_BOOL b -> Mono.P_BOOL b
  | Core.P_UNIT -> Mono.P_UNIT
  | Core.P_VID v -> Mono.P_VID (cvid v)
  | Core.P_VIDP (v, pt) -> Mono.P_VIDP (cvid v, cpatty pt)
  | Core.P_PAIR (pt, pt') -> Mono.P_PAIR (cpatty pt, cpatty pt')
and cpatty (Core.PATTY (p, ty)) = Mono.PATTY (cpat p, cty ty)

let rec cexp e = match e with
    Core.E_INT n -> Mono.E_INT n
  | Core.E_BOOL b -> Mono.E_BOOL b
  | Core.E_UNIT -> Mono.E_UNIT
  | Core.E_PLUS -> Mono.E_PLUS
  | Core.E_MINUS -> Mono.E_MINUS
  | Core.E_MULT -> Mono.E_MULT
  | Core.E_EQ -> Mono.E_EQ
  | Core.E_NEQ -> Mono.E_NEQ
  | Core.E_VID v -> Mono.E_VID (cvid v)
  | Core.E_FUN ml -> Mono.E_FUN (List.map cmrule ml)
  | Core.E_APP (et, et') -> Mono.E_APP (cexpty et, cexpty et')
  | Core.E_PAIR (et, et') -> Mono.E_PAIR (cexpty et, cexpty et')
  | Core.E_LET (d, et) -> Mono.E_LET (cdec d, cexpty et)
and cexpty (Core.EXPTY (e, ty)) = Mono.EXPTY (cexp e, cty ty)

and cmrule (Core.M_RULE (pt, et)) = Mono.M_RULE (cpatty pt, cexpty et)

and cdec d = match d with 
    Core.D_VAL (pt, et) -> Mono.D_VAL (cpatty pt, cexpty et)
  | Core.D_REC (pt, et) -> Mono.D_REC (cpatty pt, cexpty et)
  | Core.D_DTYPE -> Mono.D_DTYPE

let cprogram (dl, et) = (List.map cdec dl, cexpty et)

(* val core2mono : Core.program -> Mono.program *)
let core2mono core = 
  let mcore = mprogram core in
  let pcore = 
    if checkpoly mcore 
    then pprogram Dict.empty mcore
    else let _ = print_endline "Program has no polymorphism" in mcore 
  in
  let ccore = cprogram pcore in
  ccore


