let init = Random.self_init ()

let maxrows,
  maxcols,
  formula_value_ratio,
  sheet_fn,
  maxupdate,
  focus_ratio,
  user_fn =
  match List.tl (Array.to_list Sys.argv) with
  | [r;c;fr;fn;maxupdate;focusr;fn'] ->
    (try int_of_string r,
      int_of_string c,
      int_of_string fr,
      fn,
      int_of_string maxupdate,
      int_of_string focusr,
      fn'
     with _ -> failwith "bad arguments")
  | _ ->
    failwith "bad arguments"

let largeInt m = 1 + Random.int m
let nbrows = largeInt maxrows
let nbcols = largeInt maxcols
let updates = largeInt maxupdate

let announce =
  Printf.printf
    "Generating a sheet with %d rows and %d cols.\nGenerating %d updates.\n%!"
    nbrows nbcols updates

let random start stop =
  start + Random.int (stop - start)

let random_value () = string_of_int (Random.int 256)

let random_formula () =
  let rstart = Random.int nbrows and cstart = Random.int nbcols in
  let rstop = random rstart nbrows and cstop = random cstart nbcols in
  Printf.sprintf "=#(%d, %d, %d, %d, %s)"
    rstart cstart rstop cstop (random_value ())

let random_cell () =
  if Random.int formula_value_ratio = 0 then
    random_formula ()
  else
    random_value ()

let rec separated_list sep elem (out : string -> unit) = function
  | 0 -> assert false
  | 1 -> elem ()
  | n -> elem (); out sep; separated_list sep elem out (n - 1)

let random_row out = separated_list ";" (fun () -> out (random_cell ())) out
let random_sheet out = separated_list "\n" (fun () -> random_row out nbcols) out

let random_cell_update () =
  let r = Random.int nbrows and c = Random.int nbcols in
  let s = if Random.int 5 = 0 then
    random_formula ()
  else
    random_value ()
  in
  Printf.sprintf "%d %d %s" r c s

let random_focus () =
  let rstart = Random.int nbrows and cstart = Random.int nbcols in
  let rstop = random rstart nbrows and cstop = random cstart nbcols in
  Printf.sprintf "! %d %d %d %d" rstart cstart rstop cstop

let random_update () =
  if Random.int focus_ratio = 0 then
    random_focus ()
  else random_cell_update ()

let random_user out =
  separated_list "\n" (fun () -> out (random_update ())) out

let main =
  let cout = open_out sheet_fn in
  let user_cout = open_out user_fn in
  random_sheet (output_string cout) nbrows;
  random_user (output_string user_cout) updates;
  close_out cout;
  close_out user_cout

