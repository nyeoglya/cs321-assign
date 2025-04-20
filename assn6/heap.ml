exception InvalidLocation
exception Test
            
type loc = int 
type 'a heap = (loc * 'a) list
                          
let empty = []

let allocate h v = 
  let l = List.length h + 1
  in
  ((l, v) :: h, l )
    
let deref h l =
  try
    match (List.find (fun (l', _) -> l' = l) h) with
      (_, v) -> v
  with Not_found -> raise InvalidLocation  

let update h l v = 
  try
    match (List.find (fun (l', _) -> l' = l) h) with
      (l', _) -> List.map (fun (l_old, v_old) -> if l' = l_old then (l, v) else (l_old, v_old)) h 
  with Not_found -> raise Test  

