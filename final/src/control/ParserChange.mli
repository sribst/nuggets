(** /!\ should be in a separate file in model *)
(** type of changes *)
(** `Focus is when we have to change the focus on *)
(** where the evaluation should be prioritize first *)
(** [`update c,v]  a the coordinate [c] with [v] *) 
type changes = [
  `Focus of Coord.t * Coord.t
| `Update of Coord.t * CellValue.t
]

(** parse all update to apply from a file *) 
val parse : string -> changes list 
