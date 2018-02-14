type chan

(** [openfiles data big_data] open with Unix file [data] and *)
(** [big_data] and return a channel needed by get and set  *)
(** the channel have been separated of the file interface so we can *)
(** use it with different file without having to create a new file *)
(** interface *)
(** [data] is the file where data smaller than a specified size will *)
(** be written and [big_data] where data bigger than that that will *)
(** be written  *)
val openfiles : data:string -> big_data:string -> chan

(** [closefiles] close all file of a channel *)
val closefiles : chan -> unit

(** Module used by the functor Make *) 
module type I = sig

  (** unique key for any value *) 
  type key

  (** transform a key into an offset *)
  val to_position : key -> int

  (** type of data saved by the file interface *)
  type value

  (** how to serialize a value, to make sense most of the serialized
  value should be smaller than max_byte *)
  val serialize   : value -> bytes

  (** how to deserialize a value *) 
  val deserialize : bytes -> value
    
  (** [max_byte] is used to set the size of a serialized value written
  into the file [data] of a channel. Serialize value bigger than that
  will be written in the file [big_data] of a channel *) 
  val max_byte : int
end

(** signature of a file interface same key/value as the signature I *)
module type S = sig
  type key
  type value
  val get : chan -> key -> value
  val set : chan -> key -> value -> unit
end

(** functor to create a file interface *)   
module Make :
  functor (A:I) -> S with type key = A.key and type value = A.value

