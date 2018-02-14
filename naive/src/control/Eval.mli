module C = Coord
module V = CellValue

(**type d'une spreadsheet *)
type 'a t 

(** spreadsheet d'entrée *)
type base

(** spreadsheet de sortie de l'évaluation*)
type result

(** spreadsheet des valeurs non évalué (cycle) *)
type fail

(** type de la fonction de log utilisé *)
type t_logf = C.t -> V.t -> (C.t -> V.t -> unit)

(** spreadsheet vide*)
val empty : base t

(** [add c f s] ajout une [f] à la clé [c]  dans [s]
    gères les dépendances si c'est une formule qui dépend d'autre cellule *)
val add : coord:C.t -> formula:V.t -> sheet:base t -> base t

(** [evaluate s] evalue toutes les cellules de [s] et retourne deux 
    spreadsheets [r] [f]
    [r] : toutes les cellules évalués
    [f] : toutes les cellules non évalués *) 
val evaluate : sheet:base t -> result t * fail t

(** [update logf sheet result_sheet c f] modifie la cellule [c] par la 
    valeur [f]. 
    [Sheet] est la feuille de base de calcul. 
    [result_sheet] est la feuille des résultats obtenue avec
    evaluate. 
    logf enregistre ce changement est les modifications provoqué.
    si logf n'est pas donné alors aucun enregistrement est effectué.
*)
val update : ?logf:t_logf -> sheet:base t -> result_sheet:result t ->
             coord:C.t -> formula:V.t -> base t * result t * fail t

(** [res c r] valeur de la cellule [c] si elle a été évalué *) 
val res :  C.t -> result t -> V.value option

(** [size s] taille de s *)
val size : 'a t -> int

(** [to_string s] retoune la string associé à l'ensemble des cellules 
    de [s] *)
val to_string : 'a t -> string 
                                                
(** *)
val iter : (C.t * V.t -> unit) -> 'a t -> unit
