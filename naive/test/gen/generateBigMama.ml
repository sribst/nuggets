let init = Random.self_init ()

let maxrows, maxcols, formula_value_ratio, output_fn =
  match List.tl (Array.to_list Sys.argv) with
  | [r;c;fr;fn] ->
    (try int_of_string r, int_of_string c, int_of_string fr, fn
     with _ -> failwith "bad arguments")
  | _ -> failwith "bad arguments"

let largeInt m = 1 + Random.int m
let nbrows = largeInt maxrows
let nbcols = largeInt maxcols

let announce =
  Printf.printf
    "Generating a sheet with %d rows and %d cols.\n%!"
    nbrows nbcols

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

let main =
  let cout = open_out output_fn in
  random_sheet (output_string cout) nbrows;
  close_out cout

