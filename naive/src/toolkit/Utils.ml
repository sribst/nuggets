let get_args () = List.tl (Array.to_list Sys.argv)

let print_and_exit s = print_endline s; exit 0

let timed name f =
  let time = Unix.time () in
  let res = f () in
  let delta = Unix.time () -. time in
  Printf.printf "%s done in: %f\n" name delta;
  res

    
