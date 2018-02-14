external serialize:string -> int = "caml_serialize"
external get_valeur: int -> int -> int -> int = "get_valeur"
external get_formula: int -> int * int * int * int * int = "get_formula_o"
external set_valeur:  int -> int -> int -> int -> unit = "set_valeur"
external set_formula_o: int -> int -> int -> int * int * int * int * int -> unit = "set_formula_o"



                                                                                     
let to_formula c1 r1 c2 r2 valeur  =
  CellValue.Count((Coord.of_int c1 r1), (Coord.of_int c2 r2), valeur) 

let of_formula  = function
  | CellValue.Count(crd1,crd2,v) ->
     let c1,r1 = (Coord.to_int crd1) in
     let c2,r2 = (Coord.to_int crd2) in
     (c1,r1,c2,r2,v)

let print_f = function
  | CellValue.Formula  f ->
     let (c1,r1,c2,r2,valeur) = of_formula f in
     Printf.printf " c1 : %d r1 : %d c2 : %d r2 : %d valeur : %d \n"
                   c1 r1 c2 r2 valeur
  | CellValue.Value value -> 
     Printf.printf " valeur : %d \n" value  
       
let get row_length coord =
  let x,y = Coord.to_int coord in  
  let value = (get_valeur x y row_length) in
  if value < 0 then
    let (c1,r1,c2,r2,valeur) = get_formula (value * -1) in
    CellValue.Formula (to_formula c1 r1 c2 r2 valeur)
  else
    CellValue.Value value
                    
let set row_length coord value =
  let x,y = Coord.to_int coord in 
  match value with
  | CellValue.Value v -> 
     set_valeur x y row_length v
  | CellValue.Formula f ->
     set_formula_o x y row_length (of_formula f)
                                                                                    
