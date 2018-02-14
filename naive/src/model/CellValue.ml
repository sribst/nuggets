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
