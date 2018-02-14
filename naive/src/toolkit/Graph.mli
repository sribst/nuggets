(** Data structure used by the defi 1 for a "naive" implementation *)


(** 
    OrderedType est utilisé afin d'ordonner les valeurs du graphe.
*)
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

(** Module des valeurs *)
module type Value = sig
  type t
end

(** le functor Make crée un graphe ordonné *)
module Make :
  functor (K : OrderedType) (V:Value) ->
  sig

    (** type des clefs *)
    type key = K.t

    (** type des valeurs *)
    type value = V.t

    (** module utilisé pour les dépendances d'un noeud*)
    module KSet : Set.S with type elt = K.t

    (** le type des éléments enregistrés dans le graphe*) 
    type node = {
      value    : value option;(** Valeur du noeud *)
      children : KSet.t; (** clefs des noeuds qui dépendent de celui ci*)
      parents  : KSet.t; (** clefs des noeuds dont ce noeud dépend *)
    }

    (** type du graphe *)
    type t

    (** graphe vide  *)
    val empty : t

    (** taille du graphe *)
    val size : t -> int

    (** [fold f g acc] : fold l'ensemble du graphe avec la function f *)
    val fold : (key -> node -> 'a -> 'a) -> t -> 'a -> 'a

    (** [add k n g] ajoute le noeud [n] au graphe [g] à la clée [k], si [k] avait déjà
        un noeud associé, celle ci sera écrasée *)
    val add  : key -> node -> t -> t

    (** [add_or_update k v g] si [k] est déjà associé à un noeud
        celui-ci est mis à jour avec [v] sinon un nouveau noeud sans
        dépendance est ajouté *)
    val add_or_update  : key -> value -> t -> t

    (** [find k g] retourne le noeud associé à [k] dans [g] 
        si aucun noeud n'est associé lève Not_found *)
    val find : key -> t -> node

    (** [mem k t] vrai si un noeud est associé à [k] *)
    val mem : key -> t -> bool

    (** [union f g g'] nouveau graphe avec l'union des clés de [g] et
        [g']. quand g et g' possedent des noeud associés à une même
        clé, le noeud du nouveau graphe est décidé par [f] *)
    val union : (key -> node -> node -> node) -> t -> t -> t

    (** [bindings g] list de tous les éléments du graphe en ordre
        croissant *)
    val bindings : t -> (key * node) list

    (** [value n] : valeur de [n] *)
    val value : node -> value option

    (** [parent n] : set des parents de n *)
    val parents : node -> KSet.t

    (** [children n] : set des enfants de n*)
    val children : node -> KSet.t

    (** [update_value n v] nouveau noeud avec les même dependances que
        [n], et de valeur [v]
    *)
    val update_value : node -> value -> node

    (** [create_node v] nouveau noeud aux dépendances vide, de valeur [v]*)
    val create_node : value -> node

    (** [find_or_empty k g] retourne le noeud asoscié à [k] ou un
        nouveau noeud vide (None pour valeur)*) 
    val find_or_empty : key -> t -> node

    (** [add_edge src tgt graph] :
        ajoute une arrete entre le noeud de clef [src]
        et le noeud de clef [tgt]
    *) 
    val add_edge : src:key -> tgt:key -> graph:t -> t

    val add_edges : srcs:key list -> tgt:key -> graph:t -> t 

    val remove_children : key:key -> node:node -> graph:t -> t

    val remove_parents : key:key -> node:node -> graph:t -> t

    val remove : key:key -> node:node -> graph:t -> t

    val list_no_parents : graph:t -> (key * node) list

    val list_no_parents_omitting : omitting:key -> graph:t -> (key * node) list

    val list_no_children : graph:t -> (key * node) list

    (** TODO *) 
    val minimize_parents : t -> t


  end

