exception NotImplemented 
exception Stuck

let rec step e = raise Stuck         

let stepOpt e = try Some (step e) with Stuck -> None

let rec multiStep e = try multiStep (step e) with Stuck -> e

let stepStream e =
  let rec steps e = 
    match (stepOpt e) with 
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)



