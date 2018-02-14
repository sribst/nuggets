
(* type of values describing the resulting state of an evaluation.*)
type state

(* [find get c state] returns the content of the coordinate c. *)
val find : get:(Coord.t -> CellValue.t) ->
  coord:Coord.t ->
  state:state ->
  CellValue.t

(* [evaluate get formulas] evaluates the given formulas and return the
   resulting state. *)
val evaluate : get:(Coord.t -> CellValue.t) ->
  formulas:(Coord.t * CellValue.formula) list ->
  state

(*[update get set state changes] runs the given updates and returns the
  resulting state. *)
val update : get:(Coord.t -> CellValue.t) ->
  set:(Coord.t -> CellValue.t -> unit) ->
  state:state ->
  changes:ParserChange.changes list ->
  state
