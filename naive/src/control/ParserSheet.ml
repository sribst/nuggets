(** Parser used by the defi 1 for a "naive" implementation *)
open Angstrom 

let parens p = char '(' *> p <* char ')'

let equals = char '='
let hash   = char '#'
let comma  = char ','
let sep    = char ';'
let newline = char '\n'
let integer =
  let is_integer = function
    | '0'..'9' -> true
    | _ -> false
  in take_while1 is_integer >>| int_of_string

   
let value = integer >>| fun n -> CellValue.Value n

let formula0 =
  integer >>= fun r1 ->
  comma   *>
  integer >>= fun c1 ->
  comma   *>
  integer >>= fun r2 ->
  comma   *>
  integer >>= fun c2 ->
  comma   *>
    integer >>= fun v ->
  let c  = Coord.of_int r1 c1 in
  let c' = Coord.of_int r2 c2 in
  return CellValue.(Formula (Count(c,c',v)))

let formula = equals *> hash *> parens formula0

let cellvalue = (formula <|> value) 

let rowvalue = sep_by sep cellvalue
        
let parse_row str =
  match parse_only rowvalue (`String str) with
  |Ok v -> v
  | Error msg -> failwith msg

let fill_newsheet ll =
  let add_row (acc, i) row =
    let (acc, _) =
        List.fold_left (fun (acc, j) formula ->
            let coord = Coord.of_int i j in
            let acc = Eval.add ~coord ~formula ~sheet:acc in
            (acc, j + 1) ) (acc, 0) row
    in
    (acc, i+1)
  in
  List.fold_left (add_row) (Eval.empty, 0) ll

let parse filename =
  let file = Pervasives.open_in filename in
  let rec aux () =
    try
      let row_str = Pervasives.input_line file in
      let f_list  = parse_row row_str          in
      [f_list] @ aux ()
    with End_of_file -> []
  in
  let (sheet, _)  = fill_newsheet (aux ()) in
  Pervasives.close_in file;
  sheet 
    

           
