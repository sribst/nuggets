(** module with all the function to parse a csv *)
module A = struct
  open Angstrom 

  let parens p = char '(' *> p <* char ')'

  let equals = char '='
  let hash   = char '#'
  let comma  = char ','
  let space  = char ' '
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
    space   *> 
    integer >>= fun c1 ->
    comma   *>
    space   *> 
    integer >>= fun r2 ->
    comma   *>
    space   *> 
    integer >>= fun c2 ->
    comma   *>
    space   *> 
    integer >>= fun v ->
    let c  = Coord.of_int r1 c1 in
    let c' = Coord.of_int r2 c2 in
    return CellValue.(Formula (Count (c,c',v)))

  let formula = equals *> hash *> parens formula0

  let cellvalue = (formula <|> value)

  let rowvalue = sep_by sep cellvalue

  let parse_row str =
    match parse_only rowvalue (`String str) with
    | Ok v -> v
    | Error msg -> failwith msg
                     
end

(**[fill_row set l_form row row_list] add all the CellValue.t in *)
(**[row_list] with the function set and all formula encounter to l_form *) 
let fill_row set l_form row row_list =
  let add_formula (l_form, col) formula =
    let c = Coord.of_int row col in
    set c formula;
    let l_form =
      match formula with
      |CellValue.Value _   -> l_form
      |CellValue.(Formula (Count(c1,c2,_) as f)) ->
        (c, f) :: l_form
    in
    (l_form, col+1)
  in
  let l_form, _ = List.fold_left (add_formula) (l_form, 0) row_list in
  l_form

let parse filename set =
  let file = Pervasives.open_in filename in

  (** return a list of CellValue of the next line *) 
  let get_row () = A.parse_row (Pervasives.input_line file) in
  
  let fill l_form r row = fill_row set l_form r row in
  
  let rec aux l_form r =
    try
      let l_form = fill l_form r (get_row ()) in
      aux l_form (r + 1)
    with End_of_file -> l_form, r
  in
  
  let l_form, max_row = aux [] 0 in
  (** Le parseur renvoie la liste des formules et une fonction
  calculant les coordonnées max selon un max col donnée  **)
  (fun x -> Coord.of_int (max_row - 1) x), l_form
