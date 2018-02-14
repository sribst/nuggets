exception BiggerThanExpected

type t = bytes

val length : t -> int

val serialize : int -> t

val deserialize : t -> int

val is_flagged : t -> int -> bool

val flag : t -> int -> t

val unflag : t -> int -> t

val fill : int -> t -> t

val unfill : t -> t

val flag_with_size : t -> t

val size_flag : char -> int

val flag_last : t -> t

val unflag_last : t -> t

val is_flagged_last : t -> bool
