open Logger
(** call the functor FileInterface.Make and create a new interface to *)
(** interact with a file and return it and the channel of the file to *)
(** use by get and set *)
let create_module_FI max_col max_byte =
  let module FI =
    FileInterface.Make
      (struct
        
        type key = Coord.t
	type value = CellValue.t
                     
        (**return a unique value for each coordinate *)
        let to_position coord =
          let row, col  = Coord.to_int coord in
          (row * max_col + col)

	let serialize = CellValue.serialize
	let deserialize = CellValue.deserialize
	let max_byte = max_byte 
      end)
  in
  let chan =
    FileInterface.openfiles
      ~data:"data.byte"
      ~big_data:"big_data.byte"
  in 
  (module FI : FileInterface.S with type key = Coord.t
                                and type value = CellValue.t), chan 

(** write to a csv file [output_fn] with the function [get] *)
let write_output output_fn get max_coord  =
  let file = Pervasives.open_out output_fn in
  let max_row = Coord.row max_coord  in
  let max_col = Coord.col max_coord  in
  let rec aux row col =
    if row > max_row then ()
    else
      let c = Coord.of_int row col in
      let v = get c in
      Pervasives.output_string file (CellValue.to_string v);
      if col < max_col then
        Pervasives.output_string file (";")
      else if row <= max_row then
        Pervasives.output_string file ("\n");
      Pervasives.flush_all ();
      if col < max_col then
        aux row (col + 1)
      else
        aux (row+1) 0
  in
  aux 0 0;
  Pervasives.close_out file

let write_timed name fn get max =
  Utils.timed
    name 
    (fun () -> write_output fn get max)

(** function to try the EvalConc : evaluation concurrent with focus *)
let eval_conc get set formulas user view0 max_coord = 

  (** FIRST EVALUATION *)
  let state = Utils.timed
      "eval" (fun () -> EvalConc.evaluate ~get ~formulas)
  in
  
  (** WRITE VIEW0 *)
  write_timed "writing view0"
    view0
    (fun coord -> EvalConc.find ~get ~state ~coord)
    max_coord;

  let changes = ParserChange.parse user in
  let state = EvalConc.update ~get ~set ~state ~changes in
  fun coord -> EvalConc.find ~get ~state ~coord
  
let _ =

  let args = Utils.get_args () in
  if List.length args <> 5 then (
    let msg = "[usage] ws data.csv user.txt view0.csv changes.txt final.csv" in
    Utils.print_and_exit msg);
  let data  = List.nth args 0 in
  let user    = List.nth args 1 in
  let view0   = List.nth args 2 in
  let changes = List.nth args 3 in
  let final   = List.nth args 4 in


  let max_col = Utils.nb_col_of_file data in

  let (module FI), chan = create_module_FI max_col 4 in

  let get, set = FI.get chan, FI.set chan in

  (** PARSING *)
  let max_coord, formulas = Utils.timed
    "parser" (fun () -> ParserSheet.parse data set)
  in
  let max_coord = max_coord (max_col - 1) in
  
  (** to test EvalConc *) 
  (* let get = eval_conc get set formulas user view0 max_coord in *)

  (** to test EvalWheel : not working anymore because of a dependency
      error  *)
  ignore(EvalWheel.eval_from_assoc_list get set formulas);
  
  (** WRITE FINAL_VIEW to final *) 
  write_timed "writing final" final get max_coord;

  Logger.write_log ~filename:changes;

  FileInterface.closefiles chan

