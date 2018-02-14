(**Key module used to differentiate all computation to evaluate *)
module type Key = sig
  type t
  val compare : t -> t -> int
end

(** Computation module to evaluate partial value 
    [t] is the partial value type *)
module type Computation = sig
  type t
  type data
  val equal : t -> t -> bool 
  val partial_eval : data -> t
end

(** create a Wheel to compute partial evaluation *) 
module Make :
  functor (K:Key) (C:Computation) -> sig
    type t

    (** [of_assoc_list l] create a new computional wheel based on the 
        list [l] *) 
    val of_assoc_list      : (K.t * C.data) list -> t

    (** [compute f wheel] : run f to all partial computation in the
        wheel and return the new wheel and if any changed have been
        recorded
        [f k c] return an option [c_opt], 
        if [c_opt = None] then [k] is deleted from the wheel 
        else [c_opt = Some c'] then c' is add to the cycle at key [k]
    *)
    val compute            : (K.t -> C.t -> C.t option) -> t -> (bool *t)

    (** [compute_until_done f c] : call [compute f c] until
        no change is recorded 
    *)
    val compute_until_done : (K.t -> C.t -> C.t option) -> t -> t

    (** [cardinal wheel] number of partial evaluation hold in [wheel] *)
    val cardinal : t -> int 
  end

