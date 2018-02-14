(** [parse filename get] retourne une fonction donnant les coordonées *)
(** max de la feuille selon la colone max donnée, et un liste *)
(** contenant l'ensemble des formules *) 
val parse :
  string -> (Coord.t -> CellValue.t -> unit) ->
  (int -> Coord.t) * (Coord.t * CellValue.formula) list
