type t = int * int

let of_int x y = (x, y)

let to_int (x, y) = (x, y)

let row (x, _) = x

let col (_,y) = y

let to_string (x, y) =
  string_of_int x ^ ", "
  ^ string_of_int y

let compare (x, y) (x', y') =
  let r = x - x' in
  if r = 0 then y - y' else r

let compare_range ((r1,c1),(r2,c2)) ((r1',c1'),(r2',c2'))=
  Pervasives.compare ((r2-r1)*(c2-c1)) ((r2'-r1')*(c2'-c1'))

let is_in_range (cx, cy) ((x, y),(x', y')) =
  x <= cx && y <= cy && cx <= x' && cy <= y'

let size_range (r1,c1) (r2,c2) =
  (r2 - r1) * (c2 -c1)

let coords_count (x1,y1) (x2,y2) =
  (x2 - x1 + 1) * (y2 - y1 + 1)

let list_all (min_x,min_y) (max_x,max_y) =
  let rec aux x y r =
    if x <= max_x && y <= max_y then
        aux (x+1) (y) ((x,y)::r)
    else if y <= max_y then
      aux min_x (y + 1) r
    else r
  in
  aux min_x min_y []

let coord_stream (min_x,min_y) (max_x,max_y) =
  let x, y = (ref min_x), (ref min_y) in
  let rec f () =
    if !x <= max_x && !y <= max_y then
      let c = !x, !y in incr x; Some c
    else if !y <= max_y then begin
      x := min_x; incr y; f ()
    end
    else None
  in
  Stream.from (fun _ -> f ())

let coord_lwt_stream (min_x,min_y) (max_x,max_y) =
  let x, y = (ref min_x), (ref min_y) in
  let rec f () =
    if !x <= max_x && !y <= max_y then
      let c = !x, !y in incr x; Some c
    else if !y <= max_y then begin
      x := min_x; incr y; f ()
    end
    else None
  in
  Lwt_stream.from_direct f

let split_hor (r,c) (r',c') =
  let r1 = (r' -r)/2 + r in
  let (c1,c2),(c3,c4) =
    ((r,c),(r1,c')),
    ((r1+1,c),(r',c'))
  in
  (c1,c2),(c3,c4)

let split_ver (r,c) (r',c') =
  let c1 = (c' -c)/2 + c in
  let (c1,c2),(c3,c4) =
    ((r,c),(r',c1)),
    ((r,c1+1),(r',c'))
  in
  (c1,c2),(c3,c4)

let row_is_inf (x, _) (x', _) = x < x'
let col_is_inf (_, y) (_, y') = y < y'

let next_row (x, y) = (x+1, y)
let next_col (x, y) = (x, y+1)

let serialize (x, y) =
  let x_s = Serial.serialize x in
  let y_s = Serial.serialize y in
  let x_s = Serial.flag_with_size x_s in
  let y_s = Serial.flag_with_size y_s in
   x_s ^ y_s

let deserialize s =
  let size_x = Serial.size_flag (String.get s 0) in
  let x_s = String.sub s 1 size_x in
  let size_y = Serial.size_flag (String.get s (size_x + 1)) in
  let y_s = String.sub s (size_x+2) size_y in
  Serial.(deserialize x_s, deserialize y_s)
