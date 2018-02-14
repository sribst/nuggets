
(** partial type of a count formula being evaluate *) 
type partial_t =
  {
    (** list of stream of Coord.t and the number of value left in
        that stream (used for equality) *)
    stream_l : (Coord.t Lwt_stream.t * int ) list;
    (**partial value evaluated so far *) 
    partial : CellValue.value Lwt.t;
    (** [v] value of the formula (Count (_,_,v)) *)
    value : CellValue.value
  }

module PartialCellValue = struct
  (**type of entry*)
  type data = CellValue.formula

  type t = partial_t
    
  (** maximum size of a stream = maximum of coordinate to be browse by
      a thread *) 
  let max_size_range = 5000
  let max_thread = 512
  (** when two partial value are equal is when no stream has change in
  size (or being deleted) *) 
  let equal partial partial' =
    let f_exist (_,left) (_,left') = left <> left' in
    let sl = partial.stream_l in
    let sl' = partial'.stream_l in
    List.length sl <> List.length sl'
    || not (List.exists2 f_exist sl sl')

  let split_until_minimal c1 c2 =
    let split c1 c2 = 
      let (c1,c2), (c3,c4) = Coord.split_hor c1 c2 in
      Coord.split_ver c1 c2 , Coord.split_ver c3 c4
    in
    let rec aux n c1 c2 =
      if Coord.size_range c1 c2 < max_size_range || n >= max_thread then
        [Coord.coord_lwt_stream c1 c2, Coord.size_range c1 c2]
      else
        let n = n + 4 in 
        let ((c1,c2),(c3,c4)),((c5,c6),(c7,c8)) = split c1 c2 in
        aux n c1 c2 @ aux n c3 c4 @ aux n c5 c6 @ aux n c7 c8
    in
    aux 1 c1 c2
      
  (** transform a formula into a partial_value [t] 
      create as many stream as needed to evaluate that formula any
      stream has less than [max_size_range] coordinate in it 
  *)
  let partial_eval = function 
    | CellValue.Count (c1, c2, v) ->
      let pe =
        {stream_l = split_until_minimal c1 c2 ;
         partial = Lwt.return 0 ;
         value = v}
      in
      Printf.printf "[DEBUG] formula with %d thread \n%!"
        (List.length pe.stream_l);
      pe
end

(** create a wheel to compute PartialCellValue *)
module W = Wheel.Make (Coord) (PartialCellValue)

(** type of an evaluation (just wheel) *) 
type t = W.t
                   
(** browse a stream and return at the first formula or empty stream
    return the stream and the number of [v] in the evaluated range
*) 
let eval_count get s left v  =
  let rec aux left n =
    let%lwt c_opt = Lwt_stream.peek s in
    match c_opt with
    | None -> Lwt.return ((s,left), n)
    | Some c ->
      match get c with
      | CellValue.Value v' when v' = v ->
        ignore(Lwt_stream.junk s);
        aux (left-1) (n+1)
      | CellValue.Value v' ->
        ignore(Lwt_stream.junk s);
        aux (left-1) n
      | _ -> Lwt.return ((s,left), n)
  in
  aux left 0

(** start [eval_count] on all stream hold in [stream_l] *)
(** update the partial [value] and the stream list [stream_l] *) 
let count get set coord (partial_c:partial_t) =
  Printf.printf "lauching %d concurent thread\n%!" (List.length partial_c.stream_l) ;
  let f_eval (s,left) = eval_count get s left partial_c.value in
  let%lwt sl =  Lwt_list.map_s f_eval partial_c.stream_l in
  (* Printf.printf "done thread, now cleaning\n%!"; *)
  let f_fold n (_,n') = Lwt.return (n + n') in
  let%lwt n = partial_c.partial in
  let%lwt n = Lwt_list.fold_left_s f_fold n sl in

  let f_filter ((s,n),_) =
    let%lwt b = Lwt_stream.is_empty s in
    if b then Lwt.return None
    else Lwt.return (Some (s,n))
  in
  let%lwt sl = Lwt_list.filter_map_s f_filter sl in

  Lwt.return (
    if sl = [] then(
      set coord (CellValue.Value n);
      None)
    else (
      Some (
        {partial_c with
         stream_l = sl ;
         partial = Lwt.return n ;})))

(** evaluate until no value change or when the wheel is empty *) 
let eval get set wheel =
  let f c partial_c =
    Lwt_main.run (count get set c partial_c)
  in
  let before = W.cardinal wheel in
  let wheel = W.compute_until_done f wheel in
  let after = W.cardinal wheel in
  Printf.printf "total evaluated : %d/%d\n%!"  (before - after) before;
  wheel

let eval_from_assoc_list get set f_list =
  Printf.printf "[DEBUG] init wheel\n%!";
  let wheel = W.of_assoc_list f_list in
  Printf.printf "[DEBUG] done creating wheel\n%!";
  eval get set wheel
