exception BiggerThanExpected

type t = bytes

let length = String.length

let serialize i =
  let rec aux i s =
    if i = 0 then s else 
      let i' = i  lsr 8 in
      let i' = (i' lsl 8) lxor i in
      aux (i lsr 8) (String.make 1 (Char.chr i') ^ s)
  in
  if i = 0 then String.make 1 (Char.chr 0)
  else aux i ""

let deserialize s =
  let n = String.length s in
  let rec aux i m =
    if m = n then i else
      let c = String.get s m in
      let i = i lsl 8 in
      let i = i+ Char.code c in
      aux i (m+1)
  in
  aux 0 0

let is_flagged s i =
  let c = Char.code (String.get s 0) in
  (c land i) = i 

let flag s i =
  let n = Char.code (String.get s 0) in
  let c = Char.chr (i lor n) in
  String.make 1 c ^ String.sub s 1 (String.length s - 1)

let unflag s i =
  let n = Char.code (String.get s 0) in
  let c = Char.chr (i lxor n) in
  String.make 1 c ^ String.sub s 1 (String.length s - 1)

let flag_last s =
  let f = (1 lsl 7) in
  let n = Char.code (String.get s 0) in  
  let c = Char.chr (f lor n) in
  String.make 1 c ^ String.sub s 1 (String.length s - 1)

let unflag_last s =
  let f = (1 lsl 7) in
  let n = Char.code (String.get s 0) in  
  let c = Char.chr (f lxor n) in
  String.make 1 c ^ String.sub s 1 (String.length s - 1)

let is_flagged_last s =
  let f = (1 lsl 7) in
  let c = Char.code (String.get s 0) in
  (c land f) = f

let fill n s =
  let nb = String.length s in
  if nb > n then raise BiggerThanExpected else
    String.make (n - nb) (Char.chr 0) ^ s
    
let rec unfill s =
  if String.length s = 1 then
    s
  else
    let filler = Char.chr 0 in
    if String.length s = 0 then invalid_arg "unfill";
    if String.get s 0 = filler then unfill (String.sub s 1 (String.length s - 1))
    else s

let flag_with_size s =
  let size = String.length s in
  flag (fill (succ size) s) size
      
let size_flag = Char.code
