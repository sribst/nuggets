
type c = Coord.t

type value = int

and formula =
| Count of c * c * value

and t =
| Value of value
| Formula of formula

let string_of_value = function
  | n -> string_of_int n

let string_of_formula = function
  | Count (c1, c2, v) ->
     "=#("
     ^ Coord.to_string c1 ^ ", "
     ^ Coord.to_string c2 ^ ", "
     ^ string_of_value v  ^ ")"

let to_string = function
  | Value n -> string_of_value n
  | Formula f -> string_of_formula f

let formula_depends_on = function
  | Count (c1, c2, v) ->
     Coord.list_all c1 c2

let depends_on = function
  |Formula f -> formula_depends_on f
  | _ -> []

let split_formula = function
  | Count (c1, c2, v) ->
    let (c1,c1'), (c2,c2') = Coord.split_hor c1 c2 in
    let (c11,c11'),(c12,c12') = Coord.split_ver c1 c1' in
    let (c21,c21'),(c22,c22') = Coord.split_ver c2 c2' in
    let f1,f2,f3,f4 =
      Count (c11,c11',v),
      Count (c12,c12',v),
      Count (c21,c21',v),
      Count (c22,c22',v)
    in
    f1,f2,f3,f4

let serialize = function
  | Value n -> Serial.flag_with_size (Serial.serialize n)
  | Formula (Count (c1, c2, v)) ->
    let c1_s = Coord.serialize c1 in
    let c2_s = Coord.serialize c2 in
    let c1_s = Serial.flag_with_size c1_s in
    let c2_s = Serial.flag_with_size c2_s in
    let v_s  = Serial.serialize v in
    c1_s ^ c2_s ^ v_s

let deserialize s =
  let size_1  = Serial.size_flag (String.get s 0 ) in
  let len = String.length s in
  if size_1 = (len - 1) then
    let v_s = String.sub s 1 size_1 in
    Value (Serial.deserialize v_s)
  else
    let size_c1 = size_1 in
    let c1_s    = String.sub s 1 size_c1 in
    let size_c2 = Serial.size_flag (String.get s (size_c1 +1)) in
    let c2_s    = String.sub s (size_c1 + 2) size_c2 in
    let size_v  = String.length s - size_c1 - size_c2 - 2  in
    let v_s     = String.sub s (size_c1 + size_c2 + 2) size_v in
    let c1      = Coord.deserialize c1_s in
    let c2      = Coord.deserialize c2_s in
    let v       = Serial.deserialize v_s in
    Formula (Count (c1, c2, v))
