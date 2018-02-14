module C = Coord
module V = CellValue
(** Log est un module permettant l'enregistrement de changement dans

    un ficher.
    TODO : virer la séparation C et V qui sert à rien *)

exception Not_initialize

  (** permet la création d'un nouveau log celui est unique pour
      chaque programme (il y a une ref caché deriere) 
      [init filename step ()]
      filename est le nom du fichier de log ou enregistrer (si
      ce fichier existait déjà celui-ci disparaitra)
      step : tous les [step] ajout dans le log les changements sont
      répercuté dans le fichier.
      de base [filename] = "log.log"
      et [step] = 1
   *)
val init  : ?filename:string -> ?step:int -> unit -> unit
  
(** repercute ts les changements non enregistré dans le fichier 
      raise Not_initialize si il n'y a pas eu d'appel à init () -> le
      fichier n'est pas ouvert *)
val flush : unit -> unit
  
(** [add c t -> f ] rajoute une nouvelle entrée dans le log.
      La foncion [f] permet d'enregistrer dans le log les
      modifications provoqués pour ce changement
      tous [step] entrée, flush est appelé *) 
val add   : C.t -> V.t -> (C.t -> V.t -> unit)

(** flush et ferme le fichier *) 
val close : unit -> unit 
