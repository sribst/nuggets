(**
   Not use anymore 
   too slow and too big 
*)

exception EmptyHeap

(** type of key *)
type key = int

(** type of heap *) 
type 'a t 

(** empty heap *)
val empty : 'a t
    
(** [is_empty h] : is true if [h] is empty *)
val is_empty : 'a t -> bool

(** [push v k h] : [h] with [v] at key [k] *) 
val push : 'a t -> key -> 'a -> 'a t

(** [get_top h] : value at the top of the heap*)
val get_top : 'a t -> 'a

(** [pop h] : value at the top of [h], and the same heap without is *)
(** top element *)
val pop : 'a t -> 'a * 'a t 

val merge : 'a t -> 'a t -> 'a t 
