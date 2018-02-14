module C = Coord
module V = CellValue 
module SI = Serial
module H = Heap
module A = struct 
  open Angstrom 

  let parens p = char '(' *> p <* char ')'

  let equals = char '='
  let hash   = char '#'
  let comma  = char ','
  let space  = char ' '
  let sep    = char ';'
  let newline = char '\n'
  let integer =
    let is_integer = function
      | '0'..'9' -> true
      | _ -> false
    in take_while1 is_integer >>| int_of_string


  let value = integer >>| fun n -> V.Value n

  let formula0 =
    integer >>= fun r1 ->
    comma   *>
    space   *> 
    integer >>= fun c1 ->
    comma   *>
    space   *> 
    integer >>= fun r2 ->
    comma   *>
    space   *> 
    integer >>= fun c2 ->
    comma   *>
    space   *> 
    integer >>= fun v ->
    let c  = C.of_int r1 c1 in
    let c' = C.of_int r2 c2 in
    return V.(Formula (Count (c,c',v)))

  let formula = equals *> hash *> parens formula0

  let cellvalue = (formula <|> value)

  let rowvalue = sep_by sep cellvalue

  let parse_row str =
    match parse_only rowvalue (`String str) with
    | Ok v -> v
    | Error msg -> failwith msg
                     
end

let create_module_FI max_col max_byte =
  let module FI = FileInterface.Make
      (struct
        type key = Coord.t
        let to_position coord =
          let row, col = Coord.to_int coord in
          (row * max_col + col)
	type value = CellValue.t
	let serialize = CellValue.serialize
	let deserialize = CellValue.deserialize
	let max_byte = 4
      end)
  in
  let chan = FileInterface.openfiles ~data:"data.byte" ~big_data:"big_data.byte" in
  (module FI : FileInterface.S with type key = Coord.t and type value = CellValue.t),
  chan
    
(* let fill_row set heap row row_list = *)
(*   let add_formula (heap,col) formula = *)
(*     let c = C.of_int row col in *)
(*     set c formula; *)
(*     let heap = *)
(*       match formula with *)
(*       |V.Value _   -> heap   *)
(*       |V.(Formula (Count(c1,c2,_))) -> *)
(*         let size = C.size_range c1 c2 in  *)
(*         H.push heap size (c, formula) *)
(*     in *)
(*     (heap, col+1) *)
(*   in *)
(*   let h, _ = List.fold_left (add_formula) (heap,0) row_list in *)
(*   h  *)

module CSet = Set.Make(C)
    
let clean_heap heap s_cf =
  let rec aux h = 
    try
      let (coord, f), h = H.pop h in
      match f with
      | V.(Formula (Count (c1, c2, _))) ->
        let is_in c = C.is_in_range c (c1, c2) in 
        let depends = CSet.filter is_in s_cf in
        let h_out = aux h in
        if not (is_in coord) then 
          H.push h_out (CSet.cardinal depends) (depends,coord,f)
        else h_out
      | _ -> assert false (* only formula in Heap*)
    with H.EmptyHeap -> H.empty
  in
  aux heap 

let fill_row set heap s_cf row row_list =
  let add_formula (heap, s_cf, col) formula =
    let c = C.of_int row col in
    set c formula;
    let heap, l_cf =
      match formula with
      |V.Value _   -> heap, s_cf
      |V.(Formula (Count(c1,c2,_))) ->
        let size = C.size_range c1 c2 in
        H.push heap size (c, formula), CSet.add c s_cf
    in
    (heap, l_cf, col+1)
  in
  let h,s_cf, _ = List.fold_left (add_formula) (heap, s_cf, 0) row_list in
  h, s_cf

let parse filename max_byte =
  let heap = H.empty in
  let file = Pervasives.open_in filename in
  let read_line () =
    Pervasives.input_line file
  in
  let get_row () =
    A.parse_row (read_line ())
  in

  let row = get_row ()          in
  let max_col = List.length row in
  let (module FI), chan =
    create_module_FI max_col max_byte
  in
  let get, set = FI.get chan, FI.set chan in

  let fill h s_cf r row = fill_row set h s_cf r row in
  let heap, s_cf = fill heap CSet.empty 0 row in
  
  let rec aux heap l_cf r =
    try
      let heap, l_cf = fill heap l_cf r (get_row ()) in
      aux heap l_cf (r + 1)
    with End_of_file -> heap, l_cf, r
  in
  
  let heap, l_cf, max_row = aux heap s_cf 1 in
(*  let heap = clean_heap heap l_cf in *)
  (module FI : FileInterface.S with type key = Coord.t and type value = CellValue.t),
  ((get, set, chan), C.of_int max_row (max_col - 1), heap)
