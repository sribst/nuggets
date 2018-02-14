

val new_section : name:string -> ?display:bool -> ?write:bool -> unit -> unit

val log : section:string -> message:string -> unit

val write_log : filename:string -> unit

val debug : string -> unit
