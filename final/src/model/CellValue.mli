(** type d'une valeur
      pour le moment uniquement le cas des int est étudié, à terme float
      et autre type de valeur seront utilisés *)
type value =
  int (** Constructeur de type int *)

(** type des coordonées *)
type c = Coord.t

(** type d'une formule
      pour le moment nous ne possedons uniquement une formule qui
      permet de counter le nombre d'occurence d'une valeur *)
type formula =
  | Count of c * c * value

(** type d'une cellule, soit une formule soit une valeur *)
type t =
  | Value of value
  | Formula of formula

(** [to_string f] string correspondant à la formule f
      [to_string (Value n)] -> "n"
      [to_string (Formula f)] -> "=#(f)" *)
val to_string : t -> string

(** [depends_on f] : liste des coordonées dont [f] est dependant
      [depends_on (Value _)] : \[\]
      [depends_on (Formula _)] : non_empty list
 *)
val depends_on : t -> c list

(** split a formula into 4 sub-formula containing smaller range*) 
val split_formula : formula ->
  formula * formula * formula * formula

(** serialize a cellvalue into a string*) 
val serialize : t -> string

(** deserialize a string into a cellvalue (previously serialized by *)
(** [serialize] *) 
val deserialize : string -> t
