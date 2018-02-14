(** 
   Working version with intern ComputationWheel 
   Should be use tas reference to upgrade Cycle 
*) 
module type DataStructure = sig
  val get : Coord.t -> CellValue.t
  val set : Coord.t -> CellValue.t -> unit
  val all_formulas : unit -> (Coord.t * CellValue.t) list
end

module Make (D : DataStructure) = struct

  type evaluation_cell =
  | Count of int * Coord.t Stream.t * min * max
  | Value of int
  and min = int
  and max = int

  let coords_count c1 c2 =
    let x1, y1 = Coord.to_int c1 in
    let x2, y2 = Coord.to_int c2 in
    (x2 - x1 + 1) * (y2 - y1 + 1)

  let evaluation_cell f  = match f with
    | CellValue.(Formula (Count (c1,c2,what))) ->
      Count (what, Coord.coord_stream c1 c2, 0, coords_count c1 c2)
    | _ -> assert false

  module Store : sig
    type t
    val find : Coord.t -> t -> evaluation_cell
    val of_assoc_list : (Coord.t * CellValue.t) list -> t
    val fold : (Coord.t -> evaluation_cell -> 'a -> 'a) -> t -> 'a -> 'a
    val add : Coord.t -> evaluation_cell -> t -> t
    val remove : Coord.t -> t -> t
    val cardinal : t -> int
    val empty : t
  end = struct

    module CoordMap = Map.Make (Coord)

    type store = evaluation_cell CoordMap.t 
    type t =  store

    let find c m = match D.get c with
      | CellValue.Value n -> Value n
      | _ -> CoordMap.find c m

    let of_assoc_list l =
      List.fold_left (fun m (c,f) ->
	CoordMap.add c (evaluation_cell f) m
      ) CoordMap.empty l

    let fold = CoordMap.fold
    let add = CoordMap.add
    let remove = CoordMap.remove
    let empty = CoordMap.empty
    let cardinal = CoordMap.cardinal

  end

  let is_same_value i = function
    | Value i' when i = i' -> true
    | _ -> false

  let permits_changes what ev = match ev with
    | Value _ -> true
    | Count (_, _, min, max) -> min > what || max < what

  let rec count coord (changed, store) what fcs (min, max) =
    (*Printf.printf "%s w %d min %d max %d\n" (Coord.to_string coord) what min max;*)
    match Stream.peek fcs with
    | Some c ->
      (*Printf.printf "working on %s\n" (Coord.to_string c);*)
      let ev = Store.find c store in
      let same = is_same_value what ev in
      let min = if same then min + 1 else min in
      let permits = permits_changes what ev in
      let max = if permits && not same then max - 1 else max in
      let nev = Count (what, fcs, min, max) in
      let store = Store.add coord nev store in
      if permits then begin
	Stream.junk fcs;
	count coord (true, store) what fcs (min, max)
      end
      else changed, store
    | None ->
      D.set coord (CellValue.Value min);
      true, Store.remove coord store

  let evaluate_cell coord ev ((changed, store) as arg) = match ev with
    | Count (what, st, min, max) -> count coord arg what st (min, max)
    | _ -> assert false

  let rec eval store =
    let changed, store = Store.fold evaluate_cell store (false, store) in
    if changed then eval store else store

  let evaluate () =
    let cfs = D.all_formulas () in
    let store = Store.of_assoc_list cfs in
    let n = Store.cardinal store in
    (* let _ = eval store in (); *)
    let store = eval store in
    let nb_after = Store.cardinal store in 
    print_endline ("total evaluated : "
                   ^ (string_of_int (n - nb_after))
                   ^ "\nnot evaluate : " ^ string_of_int nb_after)
    (* Printf.printf "eval result : from %d to %d\n" n (Store.cardinal store) *)

end
