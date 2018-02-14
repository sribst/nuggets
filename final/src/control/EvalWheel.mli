(** This module is not finish, it's just a draft to show how we could *)
(** use Lwt. It's only a draft as of today it's not fast enough and *)
(** should be conbine with the solution done in EvalConc*)

(** Use Wheel and Lwt *)

type t

(** [eval get set wheel] evaluate all formula contain in wheel until the *)
(** wheel contain only cyclique cell *)
val eval :
  (Coord.t -> CellValue.t) ->
  (Coord.t -> CellValue.t -> unit) ->
  t -> t

(** [eval_from_assoc_list get set assoc_list] create a wheel from the *)
(** cell in [assoc_list] and call [eval] on that cycle *)
(** [assoc_list] value are of type (Coord.t, CellValue.Formula) *)
val eval_from_assoc_list :
  (Coord.t -> CellValue.t) ->
  (Coord.t -> CellValue.t -> unit) ->
  (Coord.t * CellValue.formula) list -> t
