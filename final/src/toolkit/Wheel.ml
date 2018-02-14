module type Key = sig
  type t
  val compare : t -> t -> int
end

module type Computation = sig
  type t
  type data
  val equal : t -> t -> bool
  val partial_eval : data -> t
end

module Make (K:Key) (C:Computation) = struct
  module KMap = Map.Make (K)

  type t = C.t KMap.t

  let of_assoc_list l =
    List.fold_left (fun m (k,d) ->
        KMap.add k (C.partial_eval d) m)
      KMap.empty l

  let compute (f:K.t -> C.t -> C.t option) wheel=
    let f_eval k d (b,wheel) =
      match f k d with
      | Some d' -> ((not (C.equal d' d))|| b, KMap.add k d' wheel)
      | None    -> (true, KMap.remove k wheel)
    in
    KMap.fold f_eval wheel (false, wheel)

  let compute_until_done f wheel =
    let rec aux i wheel =
      Printf.printf "turn %d\n%!" i ;
      let (changed, wheel) = compute f wheel in
      if changed then(
        Printf.printf "turn %d done \n%!" i ;
        aux (i+1) wheel)
      else (
        Printf.printf "wheel evaluate at turn %d \n%!" i ;
        wheel)
    in
    aux 0 wheel

  let cardinal = KMap.cardinal
end
