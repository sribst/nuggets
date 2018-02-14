
val get_args : unit -> string list

val print_and_exit : string -> 'a

val timed :string -> (unit -> 'a) -> 'a 

val time_since_start : unit -> float

val nb_col_of_file : string -> int
