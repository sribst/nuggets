(** 
   Not used anymore 
   Version using Heap -> way to slow and explose RAM because of
   dependency between formula 
*)
module V = CellValue
module C = Coord
module H = Heap 

module CSet = Set.Make(C)

let eval_f get = function
  | V.(Formula (Count (c1, c2, v))) ->
    let s = C.coord_stream c1 c2 in
    let rec aux n =
      try
        let c = Stream.next s in
        let i =
          match get c with
          | V.Value i -> i
          | _ -> assert false
        in
        let n = if i = v then n + 1
          else n
        in
        aux n
      with Stream.Failure -> n
    in
    aux 0
  | _ -> assert false

let eval get set heap =
  let rec evaluate l_done h_in h_out =
    if H.is_empty h_in then h_out
    else 
      let (depends,c,f), h_in = H.pop h_in  in
      if CSet.is_empty depends then
        let v = eval_f (get) f in
        set c (V.Value v);
        evaluate (CSet.add c l_done) h_in h_out 
      else
        let depends =
          CSet.filter (fun c -> not (CSet.mem c l_done)) depends
        in
        let h_out = H.push h_out (CSet.cardinal depends) (depends,c,f) in
        evaluate l_done h_in h_out 
  in
  let rec until_done heap =
    let heap = evaluate CSet.empty heap H.empty in
    if H.is_empty heap then
      heap
    else
      let (depends,c,f)  = H.get_top heap in
      if CSet.is_empty depends then
        until_done heap 
      else
        heap 
  in
  until_done heap
    
