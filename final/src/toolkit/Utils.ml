
let get_args () = List.tl (Array.to_list Sys.argv)

let print_and_exit s = print_endline s; exit 0

let timed name f =
  let time = Unix.gettimeofday () in
  let res = f () in
  let delta = Unix.gettimeofday () -. time in
  let message = Printf.sprintf "%s done in: %f" name delta in
  Logger.debug message;
  res

let start_time = Unix.gettimeofday ()
let time_since_start () = Unix.gettimeofday () -. start_time

let nb_col_of_csv line size =
    
  let rec loop value line = function
    | -1 -> value 
    | i when Char.equal (String.get line i) ';'  ->
       loop (value+1) line (i-1)
    | i -> loop value line (i-1) in
  loop 0 line size
    
let nb_col_of_file filename = 
  let file = open_in filename in
  let line = input_line file in
  let nb_col = nb_col_of_csv line ((String.length line)-1) in
  Pervasives.close_in file; (nb_col+1)

    
