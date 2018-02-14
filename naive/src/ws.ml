let write_output output_fn sheet =
  let file = Pervasives.open_out output_fn in
  Eval.iter (fun (c, v) ->
      if Coord.col c = 0 && Coord.row c <> 0 then
        Pervasives.output_string file ("\n");
      Pervasives.output_string file (CellValue.to_string v ^ ";")
    ) sheet;
  Pervasives.close_out file
  
let _ =
  let args = Utils.get_args () in
  let f_input   = List.nth args 0 in
  let f_update  = List.nth args 1 in
  let f_output  = List.nth args 2 in
  let f_log     = List.nth args 3 in

  Log.init ~filename:f_log ();
  let sheet = ParserSheet.parse f_input in
  let change = ParserChange.parse f_update in

  let result_sheet, failed_sheet = Eval.evaluate sheet in

  let update (b, r, _) (c, f) =
    Eval.update ~logf:Log.add ~sheet:b ~result_sheet:r ~coord:c ~formula:f
  in
  let sheet, result_sheet, failed_sheet =
    List.fold_left (update) (sheet, result_sheet, failed_sheet) change
  in

  write_output f_output result_sheet;
  Log.close ();
  Utils.print_and_exit ("Done !")

