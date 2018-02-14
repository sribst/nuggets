(** Type d'une coordonée de cellule *)
type t

(** [of_int row col] : nouvelle coordonnée *)
val of_int : int -> int -> t

(** [to_int c] : recupere les valeur (row, col) de [c] *)

val to_int : t -> int * int

(** row c] : ligne de [c] *)
val row : t -> int

(** [col c] : colonne de [c] *)
val col : t -> int

(** [compare c c'] Compare c c' *)
val compare : t -> t -> int

(** [compare (c1,c2) (c1',c2')] Compare the size of 2 ranges
    (c1,c2) and (c1',c2') *)
val compare_range : (t * t) -> (t * t) -> int

(** [size_range c1 c2] size of the range from c1 to c2  *)
val size_range : t -> t -> int

val coords_count : t -> t -> int

val is_in_range : t -> (t * t) -> bool

(** [list_all c c'] liste des coordonées comprises entre c et c' *)
val list_all : t -> t -> t list

val coord_stream : t -> t -> t Stream.t

(** [row_is_inf c c']  vrai si la ligne de c est inférieur à c' *)
val row_is_inf : t -> t -> bool

(** [col_is_inf c c']  vrai si la colonne de c est inférieur à c' *)
val col_is_inf : t -> t -> bool

(** [next_row c] : coordonée suivante de c par rapport par rapport à
    la ligne *)
val next_row : t -> t

(** [next_row c] : coordonée suivante de c par rapport par rapport à
    la colonne *)
val next_col : t -> t

(** [to_string c] : string correspondant à c *)
val to_string : t -> string
