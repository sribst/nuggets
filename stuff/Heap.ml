exception EmptyHeap

type key = int
type rank = int
  
type 'a heap =
  | Leaf
  | Node of 'a heap * key * 'a heap * 'a * rank 

and 'a t = 'a heap 

let empty = Leaf

let is_empty = function
  | Leaf -> true
  | _    -> false
    
let singleton k v =
  Node (Leaf, k, Leaf,  v, 1)
      
let rank_of = function
  | Leaf -> 0
  | Node (_,_,_,_,r) -> r
    
let rec merge t1 t2 =
  match t1, t2 with
  | Leaf, t | t, Leaf -> t
  | Node (l, k1, r, n, _), Node (_, k2, _, _, _) -> 
    if k1 > k2 then merge t2 t1
    else
      let merged = merge r t2 in
      let r_l = rank_of l in 
      let r_r = rank_of merged in
      if r_l >= r_r then
        Node(l, k1, merged, n,  r_l + 1)
      else
        Node(merged , k1, l, n,  r_l + 1)

let push h k v =
  merge (singleton k v) h

let get_top = function
  | Leaf -> raise EmptyHeap
  | Node (_, _, _, f, _) -> f

let del_top = function
  | Leaf -> raise EmptyHeap
  | Node (l, _, r, _, _) -> merge l r

let pop h =
  match h with 
  | Leaf   -> raise EmptyHeap
  | Node _ ->
    let f = get_top h in
    let h = del_top h in
    (f, h)
