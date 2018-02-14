(** Data structure used by the defi 1 for a "naive" implementation *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type Value = sig
  type t
end

module Make (K : OrderedType) (V:Value) = struct
  type key = K.t
  type value = V.t

  module KSet= Set.Make (K)

  module GMap = Map.Make (K)

  type node = {
      value     : value option;
      children  : KSet.t;
      parents   : KSet.t;
    }

  type t = node GMap.t

  let empty = GMap.empty

  let size = GMap.cardinal

  let fold = GMap.fold

  let add = GMap.add

  let find = GMap.find

  let mem = GMap.mem

  let bindings = GMap.bindings

  let union f g g' =
    GMap.union (fun k v v' -> Some (f k v v')) g g'

  let value n = n.value

  let parents n = n.parents

  let children n = n.children

  let update_value n v = {n with value = Some v}

  let add_child n c = {
    n with children =  KSet.(union (singleton c) n.children)
  }

  let add_parent n p = {
    n with parents =  KSet.(union (singleton p) n.parents)
  }

  let remove_child n c = {
      n with children =  KSet.(remove c n.children)
    }

  let remove_parent n p = {
    n with parents =  KSet.(remove p n.parents)
  }


  let create_node v = {
    value    = Some v;
    children = KSet.empty;
    parents  = KSet.empty;
  }

  let add_or_update c v g =
    let n =
      try update_value (find c g) v
      with Not_found -> create_node v
    in
    add c n g

  let empty_node = {
    value = None;
    children = KSet.empty;
    parents  = KSet.empty;
  }

  let find_or_empty k g =
    try find k g with Not_found -> empty_node

  let add_edge ~src ~tgt ~graph =
    let node_src = find_or_empty src graph in
    let node_src = add_child node_src tgt in
    let node_tgt = find_or_empty tgt graph in
    let node_tgt = add_parent node_tgt src in
    let graph = add src node_src graph in
    let graph = add tgt node_tgt graph in
    graph

  let add_edges ~srcs ~tgt ~graph =
    let node_tgt = find_or_empty tgt graph in
    let graph, node_tgt = List.fold_left (fun (graph, node_tgt) src ->
      let node_src = find_or_empty src graph in
      let node_src = add_child node_src tgt in
      let node_tgt = add_parent node_tgt src in
      GMap.add src node_src graph, node_tgt) (graph,node_tgt) srcs
    in GMap.add tgt node_tgt graph

(*  let link_all ~srcs ~tgt ~graph =
    List.fold_left (fun graph src -> link src tgt graph) graph srcs*)

  let remove_children ~key ~node ~graph =
    let all_c = children node in
    let graph =
      KSet.fold (fun k graph ->
        let n = find k graph in
        let n = remove_parent n key in
        add k n graph
      ) all_c graph in
    graph

  let remove_parents ~key ~node ~graph =
    let all_p = parents node in
    let graph =
      KSet.fold (fun k graph ->
          let n = find k graph in
          let n = remove_child n key in
          add k n graph
        ) all_p graph in
    graph

  let remove ~key ~node ~graph =
    let graph = remove_children ~key ~node ~graph in
    let graph = remove_parents  ~key ~node ~graph in
    GMap.remove key graph

  let list_no_parents ~graph =
    let filter = (fun _ n -> KSet.is_empty n.parents) in
    GMap.filter filter graph |> GMap.bindings

  let list_no_parents_omitting ~omitting ~graph =
    let filter =
      (fun k n -> KSet.is_empty n.parents && K.compare k omitting <> 0)
    in
    GMap.filter filter graph |> GMap.bindings

  let list_no_children ~graph =
    GMap.filter (fun _ n -> KSet.is_empty n.children) graph |> GMap.bindings

  let minimize_parents graph =
    let ks = fold (fun k _ s ->
      KSet.(union (singleton k) s)
    ) graph KSet.empty in
    fold (fun k n g ->
      let p = KSet.inter n.parents ks in
      let n = {n with parents = p} in
      add k n g
    ) graph empty
end
