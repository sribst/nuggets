module C = Coord
module V = CellValue
                  
module G = Graph.Make (C) (V)

type base
type result
type fail

type t_logf = C.t -> V.t -> (C.t -> V.t -> unit)
            
type 'a t = G.t

let empty = G.empty

let size = G.size

(** ajoute la valeur et met à jour ses parents  (cellule dont il dépend) *) 
let add ~coord ~formula ~sheet =
  let s = G.add_or_update coord formula sheet in
  let n = G.find coord s in
  let s = G.remove_parents ~key:coord ~node:n ~graph:s in
  let dl = V.depends_on formula in
  G.add_edges ~srcs:dl ~tgt:coord ~graph:s 

(** fonction de la formule Count
    [formula_count c c' v s] compte le nombre d'occurence de [v] entre [c]
    et [c'] dans [s]
 *)
let formula_count s c_start c_end v =
  let rec aux r c =
    let r =
      try
        let n = G.find c s in
        match (G.value n) with
        | None -> r
        | Some (V.Value v') -> if (v = v') then r + 1 else r  
        | _ -> assert false (* Par les propiétés des DAG *)
      with _ -> r
    in
    if C.col_is_inf c c_end then
      aux r (C.next_col c)  
    else if C.row_is_inf c c_end then
      aux r (C.of_int ((C.row c) + 1) (C.col c_start))
    else r
  in
  aux 0 c_start

(** appel la bonne fonction selon la formule 
    pour le moment uniquement Count
    TODO : modifier pour ne pas avoir le warning de partern-matching
 *)
let evaluate_formula s f =
  match f with
  | V.Value v -> v
  | V.Formula (V.Count (c_start, c_end, v)) ->
    formula_count s c_start c_end v

(** evalue un noeud et enregistre sa nouvelle valeur 
    si le noeud et vide ou contient qu'une valeur pas de modification
    effectué *) 

let evaluate_node logf result_sheet (key, node) =
  let value = G.value node in
  let node = match value with
    | Some f ->
      let v = evaluate_formula result_sheet f in
      let node = G.update_value node (V.Value v) in
      logf key (V.Value v);
      node
    | None -> node
  in
  G.add key node result_sheet

(** [evaluate_aux logf (s_out, s_in) evalue l'ensemble de la sheet 
    [s_in].
    Par "vague" : 
    - on récupère la liste de tous les noeuds sans parent (ne dépendant
      d'aucune autre cellule.
      - si la liste est vide -> fini
      - sinon on evalue tous les noeuds de cette liste et on les
        répercute dans [s_out]
        on les supprime de [s_in] et on recommence *)
let rec evaluate' logf (result_sheet, comp_sheet) =
  match (G.list_no_parents ~graph:comp_sheet) with 
  | [] -> (result_sheet, comp_sheet)
  | _ as no_deps  ->
    let result_sheet =
      List.fold_left (evaluate_node logf) result_sheet no_deps 
    in
    let comp_sheet = List.fold_left (fun graph (key,node) ->
      G.remove ~key ~node ~graph
    ) comp_sheet no_deps
    in
    evaluate' logf (result_sheet, comp_sheet)

let evaluate ~sheet =
  let logf = (fun _ _ -> ()) in
  evaluate' logf (G.empty, sheet)

let to_string s =
  let to_string_aux c n acc =
    let str_v = match G.value n with
      | None   -> "empty"
      | Some v -> V.to_string v 
    in
    acc ^ "\nnode : "
    ^ C.to_string c ^ " : "
    ^ str_v ^ "\n depends on ["
    ^ (G.KSet.fold (fun c s -> s ^ C.to_string c ) (G.parents n) "")
    ^ "]\n"
  in
  G.fold to_string_aux s ""

let to_compute k sheet =
  let rec aux k calc =
    let n = G.find k sheet in
    if G.mem k calc then calc
    else
      let calc = G.add k n calc in
      let children = G.children n in
      G.KSet.fold aux children calc
  in
  let calc = aux k G.empty in
  G.minimize_parents calc

let no_log _ _ _ _ = ()

let update ?(logf=no_log) ~sheet ~result_sheet ~(coord:C.t) ~(formula:V.t) =
  let logf    = logf coord formula in
  let b_sheet = add ~sheet ~coord ~formula in
  let s_calc  = to_compute coord b_sheet   in
  let r_sheet, f_sheet = evaluate' logf (result_sheet, s_calc) in
  b_sheet, r_sheet, f_sheet

let res c s =
  match G.value @@ G.find c s with
  | Some (V.Value n) -> Some n
  | _ -> None 

let iter f s =
  let l = G.bindings s in
  let l =
    List.fold_right (fun (k,n) acc ->
        match G.value n with
        | None   -> acc
        | Some f -> (k,f)::acc) l []
  in
  List.iter f l
