
(* the type of input/output channels *)
type t

(* [open_file filename] opens the specified filename and returns the iochannel
   might raise usual Unix errors on openning files descriptors.
*)
val open_file : string -> t

(* [close_file chan] closes the given iochannel. *)
val close_file : t -> unit

(* [iseed_set iochan pos] sets the current input position for the given
   iochannel. *)
val iseek_set : t -> int -> unit

(* same as [iseek_set] but for the output position. *)
val oseek_set : t -> int -> unit

(* [iseek_cur iochan] returns the current input position for the given
   iochannel. *)
val iseek_cur : t -> int

(* same as [iseek_cur] but for the ouput position. *)
val oseek_cur : t -> int

(* [write iochan buf from n] writes n byte from the buffer [buf] starting
   at position [from] in the iochannel [iochan] and returns the number of
   written bytes. *)
val write : t -> bytes -> int -> int -> int

(* [read iochan buf from n] reads n byte from the iochannel [iochan],
   stores them in the buffer [buf] starting at position from and return the
   numbers of read bytes.
*)
val read : t -> bytes -> int -> int -> int
