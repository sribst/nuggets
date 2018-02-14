
(* EvalConc is a module providing concurrent evaluation of a spreadsheet
*)

(* We will use the Computation module to handle concurrency.
   For more information please refer to the Computation module in the toolkit.
*)
open Computation
open Calculus.Operators


module Coordtbl = Hashtbl.Make (struct
  type t = Coord.t
  let hash = Hashtbl.hash
  let equal x y = Coord.compare x y = 0
end)

module CoordMap = Map.Make (Coord)

(* A cell is a unit holding the necessary data for the computation
   of a formula.*)
module Cell : sig

  (*[In] means that either the formula associated with the cell is in a zone
     of focus or another focused formula depends on the result of this one.
    [Out] is the opposite.
  *)
  type focus = In | Out

  (* To compute formulas we need various informations. *)
  type t = {
    (* The stream of coordinates the formula depends on. *)
    mutable stream : Coord.t Stream.t;
    (* The current minimum result we can expect from this computation. *)
    mutable min : int;
    (* current maximum expected result. *)
    mutable max : int;
    (* what are we counting.*)
    mutable what : int;
    (* what to do when we block on another formula at coordinates c. *)
    mutable on_block : Coord.t -> unit;
    (* what to do when the computation is finished. *)
    mutable on_end : unit -> unit;
    (* the id of the process associated with this cell.
       None means that this cell is not currently running. *)
    mutable id : int option;
    (* focus tells if the process associated with this cell should be
       prioritarized. *)
    mutable focus : focus;
  }

  (* various utilities functions *)
  val cell : CellValue.formula -> t
  val id : t -> int
  val update_cell : t -> t -> unit
  val reset_cell : t -> unit

  (* [cell_step store cell c v] computes one step of the computation
     of the cell and returns a boolean to tell if progress was made.
  *)
  val cell_step : t Coordtbl.t -> t -> Coord.t -> CellValue.t -> bool

end = struct

  type focus = In | Out

  type t = {
    mutable stream : Coord.t Stream.t;
    mutable min : int;
    mutable max : int;
    mutable what : int;
    mutable on_block : Coord.t -> unit;
    mutable on_end : unit -> unit;
    mutable id : int option;
    mutable focus : focus;
  }

  let cell = function
    | CellValue.(Count (c1, c2, what)) ->
      let stream = Coord.coord_stream c1 c2 in
      let min = 0 in
      let max = Coord.coords_count c1 c2 in
      let on_block _ = () in
      let on_end _ = () in
      let id = None in
      let focus = Out in
      {stream; min; max; what; on_block; on_end; id; focus}

  let update_cell cell cell' =
    cell.stream <- cell'.stream;
    cell.min <- cell'.min;
    cell.max <- cell'.max;
    cell.what <- cell'.what

  (* We read [read] at coordinates [cread].
     There are three cases :
     -We read the value we are counting
     -We read a value but it doesn't match what we count
     -We read a formula -> We need to find the partial results associated
     with this formula to decide if we can make progress or not.
*)
  let cell_step store cell cread read = CellValue.(match read with
    | Value n when n = cell.what ->
      Stream.junk cell.stream;
      cell.min <- cell.min + 1;
      true
    | Value _ ->
      Stream.junk cell.stream;
      cell.max <- cell.max - 1;
      true
    | _ ->
      let cell' = try Coordtbl.find store cread
	with _ ->
	  Printf.printf "error on %s %s\n" (Coord.to_string cread)
	    (CellValue.to_string read);
	  assert false
      in
      if cell'.min = cell'.max && cell.what = cell'.min then (
	Stream.junk cell.stream;
	cell.min <- cell.min + 1;
	true
      )
      else if cell'.min <= cell.what && cell.what <= cell'.max then
	false
      else (
	Stream.junk cell.stream;
	cell.max <- cell.max - 1;
	true
      )
  )

  let id cell = match cell.id with
    | Some id -> id
    | None -> assert false

  let reset_cell cell =
    cell.id <- None;
    cell.focus <- Out;
    cell.on_block <- (fun _ -> ());
    cell.on_end <- (fun () -> ())

end


module State : sig

  (* We represent shared memory and current formulas with a value of type t. *)
  type t = {
    store : Cell.t Coordtbl.t;
    mutable formulas : CellValue.formula CoordMap.t;
  }

  val create : (Coord.t * CellValue.formula) list -> t
  val write : t -> Coord.t -> Cell.t -> unit
  val read : t -> Coord.t -> Cell.t
  val remove : t -> Coord.t -> unit
  val iter : (Coord.t -> Cell.t -> unit) -> t -> unit
  val set_formulas : t -> CellValue.formula CoordMap.t -> unit

end = struct

  type t = {
    store : Cell.t Coordtbl.t;
    mutable formulas : CellValue.formula CoordMap.t;
  }

  let create cfs =
    let formulas = List.fold_left (fun m (c,f) ->
      CoordMap.add c f m
    ) CoordMap.empty cfs
    in
    let store = Coordtbl.create (List.length cfs) in
    {store; formulas}

  let write state = Coordtbl.replace state.store
  let remove state = Coordtbl.remove state.store
  let read state = Coordtbl.find state.store
  let iter f state = Coordtbl.iter f state.store

  let set_formulas state formulas =
    state.formulas <- formulas

end

open Cell

(* We will use a reference as flag to know if any computation could make
   progress during one step of our execution.
   This is our condition to stop the evaluation, because if no one can
   progress at some point it means no one will ever progress.
*)
let advance = ref true

(* We define what is the calculus associated with the computation of a cell.
   It reads from its associated stream and makes one step in its
   computation.
   We react and set the advance flag based on wether are blocking on
   another cell computation, we ended our caculus or we simply progressed.
*)
let unit = ret ()
let rec count store get cell = unit >>= fun () ->
  match Stream.peek cell.stream with
  | Some c ->
    let adv = Cell.cell_step store cell c (get c) in
    advance := adv || !advance;
    if adv then
      count store get cell
    else begin
      cell.on_block c;
      (* When we block on a coordinate we freeze ourself to prevent the
	 execution from trying to make us progress again until others are done.
	 Then we will eventually continue with the additional info provided
	 by the others.
      *)
      !! (count store get cell)
    end
  | None ->
    advance := true;
    cell.on_end ();
    unit

(* First evaluation *)
let run =
  let rec run b ex =
    if not (!advance) then b, ex else begin
      advance := false;
      let ex = Execution.exec_step ex in
      run (b || !advance) ex
    end
  in
  run false

let rec eval ex =
  let adv, ex = run ex in
  if adv then (advance := true; eval ex) else ex

(* Updates *)
let run_update pending =
  let rec run b ex ex_pr =
    if not (!advance) then b, ex, ex_pr else begin
      advance := false;
      let ex = Execution.exec_step ex in
      let ex_pr = Execution.exec_step ex_pr in
      let ex, ex_pr = List.fold_left (fun (ex, ex_pr) cell ->
	let pc, ex = Execution.extract_process (Cell.id cell) ex in
	let ex_pr, id = Execution.add_process ~priority:10 pc ex_pr in
	cell.id <- Some id;
	cell.focus <- In;
	ex, ex_pr
      ) (ex, ex_pr) !pending in
      pending := [];
      run (b || !advance) ex ex_pr
    end
  in
  run false

let rec eval_update pending ex ex_pr =
  let adv, ex, ex_pr = run_update pending ex ex_pr in
  if adv then (advance := true; eval_update pending ex ex_pr)
  else ex, ex_pr

(* Create the initial state and the execution for the first evaluation *)
let mk get cfs =
  let state = State.create cfs in
  let ex = List.fold_left (fun ex (c,f) ->
    let cell = Cell.cell f in
    State.write state c cell;
    let ex, id =
      Execution.add ~priority:1000 (count state.State.store get cell) ex
    in
    cell.id <- Some id;
    ex
  ) Execution.empty cfs in
  state, ex


let evaluate ~get ~formulas =
  let state, ex = mk get formulas in
  let ex = eval ex in
  let open Computation.Execution in
  let nb = IdMap.cardinal (results ex) in
  let nb' = IdMap.cardinal (running ex) in
  let nb'' = IdMap.cardinal (frozen ex) in
  let msg = Printf.sprintf
    "\nfinished : %d\nstuck    : %d\nfrozen   : %d" nb nb' nb''
  in
  Logger.debug msg;
  State.iter (fun _ cell -> reset_cell cell) state;
  state

(* Return the map of formulas depending on the value of a given coordinate *)
let impacted c cmap : CellValue.formula CoordMap.t =
  let rec impacted k imp notimp =
    if CoordMap.is_empty imp then k imp else
      let imp', notimp = CoordMap.partition (fun _ f -> match f with
	| CellValue.(Count (c1,c2,_)) ->
	  CoordMap.exists (fun c _ ->  Coord.is_in_range c (c1,c2)) imp
      ) notimp in
      let k = fun r -> k (CoordMap.union (fun _ f _ -> Some f) r imp) in
      impacted k imp' notimp
  in
  let fakecoord = Coord.of_int (-1) (-1) in
  let fake = CellValue.Count (fakecoord, fakecoord, -1) in
  impacted (fun x -> CoordMap.remove c x) (CoordMap.singleton c fake) cmap

(* Computes impacted coordinates and update the state *)
let update_state_impacted state coord form =
  let formulas = state.State.formulas in
  let formulas = CoordMap.remove coord formulas in
  let imp = Utils.timed "split" (fun () -> impacted coord formulas) in
  let formulas, imp = match form with
    | CellValue.Value _ ->
      State.remove state coord;
      formulas, imp
    | CellValue.(Formula form) ->
      let formulas = CoordMap.add coord form formulas in
      let imp = CoordMap.add coord form imp in
      formulas, imp
  in
  State.set_formulas state formulas;
  imp

let update_cell state c f =
  let cell' = Cell.cell f in
  try let cell = State.read state c in
      Cell.update_cell cell cell';
      cell
  with Not_found -> State.write state c cell'; cell'

(* Runs an update to its end *)
let run_update get set state priority_zone (coord, form) =
  

  let log_section = Printf.sprintf "%s %s"
    (Coord.to_string coord)
    (CellValue.to_string form)
  in
  Logger.new_section ~name:log_section ();


  (* We reset the flag *)
  advance := true;

  (* We compute the coordinates impacted by the change *)
  let imp = update_state_impacted state coord form in
  (* We need to write the change because it [coord] previously referenced
     a value we need to know it is now a formula when searching for it.
  *)
  set coord form;

  (* We define what to do when we are among the focused cells and we
     suddenly block on another computation.
     We want to change the priority of the cell associated with the
     coordinate we are blocking on, so we maintain a list of pending changes
     of priority to do just that when the current step of the execution will
     be achieved.
  *)
  let pending = ref [] in
  let rec on_block c =
    let cell = try State.read state c with _ -> assert false in
    match cell.id with
    | Some id when cell.focus = Out ->
      pending := cell :: !pending;
      cell.on_block <- on_block;
      cell.focus <- In;
    | _ -> ()
  in

  (* What to do when we finish our computation. *)
  let on_end c () =
    let delta = Utils.time_since_start () in
    let value = (State.read state c).min in
    let message = Printf.sprintf "%s %d finished at %f"
      (Coord.to_string c) value delta in
    Logger.log ~section:log_section ~message
  in

  (* We create two execution.
     [ex] is the execution for the cells out of the priority zone
     [ex_pr] is the execution for the cells in the priority zone
  *)
  let ex, ex_pr = CoordMap.fold (fun c f (ex, ex_pr) ->
    let cell = update_cell state c f in
    let ex, ex_pr =
      if Coord.is_in_range c priority_zone then begin
	let ex_pr, id = Execution.add
	  ~priority:10
	  (count state.State.store get cell) ex_pr
	in
	cell.id <- Some id;
	cell.on_block <- on_block;
	cell.focus <- In;
	ex, ex_pr
      end
      else begin
	let ex, id = Execution.add
	  ~priority:1
	  (count state.State.store get cell) ex
	in
	cell.id <- Some id;
	ex, ex_pr
      end in
    cell.on_end <- on_end c;
    ex, ex_pr
  ) imp (Execution.empty, Execution.empty) in
  ignore (eval_update pending ex ex_pr);
  State.iter (fun _ cell -> reset_cell cell) state

let update ~get ~set ~state ~changes =
  let run = run_update get set state in
  let rec update priority_zone changes = match changes with
  | `Focus (c1, c2) :: changes ->
    let name = Printf.sprintf "focus on %s %s"
      (Coord.to_string c1) (Coord.to_string c2) in
    Logger.new_section ~name ();
    Logger.debug name;
    let priority_zone = c1, c2 in
    update priority_zone changes
  | `Update (c, f) :: changes ->
    let message = Printf.sprintf "update %s %s"
      (Coord.to_string c) (CellValue.to_string f) in
    Logger.debug message;
    run priority_zone (c, f);
    update priority_zone changes
  | [] -> state
  in
  let priority_zone = (Coord.of_int (-1) (-1), Coord.of_int (-1) (-1)) in
  update priority_zone changes


type state = State.t

(* [find get coord state] finds the cell value at [coord]
   We first try to find it in the execution state and when
   we failed we look for it in the data structure representing the
   spreadsheet.
*)
let find ~get ~coord ~state =
  try
    let cell = State.read state coord in
    if cell.min = cell.max then CellValue.Value cell.min
    else CellValue.Formula (CoordMap.find coord state.State.formulas)
  with Not_found -> get coord
