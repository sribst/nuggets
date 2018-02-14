exception Not_initialize
module C = Coord
module V = CellValue

module State = struct
  type t = {
    coord   : C.t;
    formula : V.t;
  }
  let to_string cv =
    "value : " ^ C.to_string cv.coord
    ^ " = " ^ V.to_string cv.formula
  let create coord formula = {coord; formula;}
end

let write_in str file = Pervasives.output_string file str 
    
let time = ref 0

type state = (State.t)

module TMap=
  Map.Make (struct
    type t = int * state
    let compare (t, _) (t', _) = compare  t t'
  end )

type t = {
  mutable map        : (state list) TMap.t ;
  mutable file       : (Pervasives.out_channel) option;
  mutable step       : int;
  mutable last_flush : int;
}    

let log = {
  map         = TMap.empty;
  file        = None ;
  step        = 10;
  last_flush  = 0;
}

let init ?(filename = "log.log") ?(step = 1) () =
  log.step <- step;
  log.file <- Some (Pervasives.open_out filename)
      
let str_of_key (t, v) =
  "change number "
  ^ string_of_int t ^ " "
  ^ State.to_string v
  ^ " =>\n"

let str_of_state v = "    - " ^ State.to_string v ^ "\n" 

(**  
     m : map des changements non écrit sur le fichier encore
     l : (key,value) list trié croissant
     on itere ensuite sur la liste pour creer les string et les
     ecrire dans le fichier 
*) 
let flush () =
  let m = TMap.filter (fun (t, _) _ -> t >= log.last_flush) log.map in
  let l = TMap.bindings m in
  let str_aux acc s = acc ^ str_of_state s in
  let str (k, sl) =
    str_of_key k ^ (List.fold_left str_aux "" sl) ^ "\n"
  in
  match log.file with
  | None      -> raise Not_initialize
  | Some file -> 
    List.iter (fun ksl -> write_in (str ksl) file) l;
    log.last_flush <- !time

let add_in tc c f =
  let sl = TMap.find tc log.map in
  log.map <- TMap.add tc ((State.create c f)::sl) log.map

let add c f =
  if (!time) mod log.step = 0 then flush ();
  let k = (!time, State.create c f ) in 
  log.map <- TMap.add k [] log.map;
  time := !time + 1; 
  add_in k

let close () =
  flush ();
  match log.file with 
  | None      -> raise Not_initialize
  | Some file -> Pervasives.close_out file 
