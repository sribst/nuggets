
module Calculus : sig

  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a
  val step : 'a t -> 'a t
  val nstep : int -> 'a t -> 'a t
  val freeze : 'a t -> 'a t
  val defreeze : 'a t -> 'a t

  module Operators : sig
    (* bind *)
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (* return *)
    val ret : 'a -> 'a t
    (* freeze *)
    val ( !! ) : 'a t -> 'a t
    (* defreeze *)
    val ( ?? ) : 'a t -> 'a t
  end

end

module Execution : sig
  module IdMap : Map.S with type key = int
  type priority = int
  type id = IdMap.key
  type 'a process
  type 'a execution
  val empty : 'a execution
  val add : ?priority:priority -> 'a Calculus.t -> 'a execution -> 'a execution * id
  val extract_process : id -> 'a execution -> 'a process * 'a execution
  val add_process : ?priority:priority -> 'a process -> 'a execution -> 'a execution * id
  val exec_run  : 'a execution -> 'a execution
  val exec_step : 'a execution -> 'a execution
  val results   : 'a execution -> 'a IdMap.t
  val frozen    : 'a execution -> 'a Calculus.t IdMap.t
  val running   : 'a execution -> 'a Calculus.t IdMap.t
  val modify_priority : id -> priority -> 'a execution -> 'a execution
end
