(* 
 * EXERCISE 5
 * Playing around with strings
 *
 * sin(x) can be approximate by the Taylor's series. Similarly you can
 * approximate all the trigonometric and transcendent functions (look at
 * http://en.wikipedia.org/wiki/Taylor_series).
 * Let's write a module to implement sin x n by using the Taylor's series (where
 * n is the level of approximation, i.e., 1 only one item, 2 two items, 3 three
 * items and so on).
 * Do the same with cosine, tangent, logarithm and so on.
 * Let's compare your functions with those implemented in the pervasive module
 * at the growing of the approximation level.
 *)

let abs_string n =
  if n >= 0.
  then "- " ^ string_of_float (n) 
  else "+ " ^ string_of_float (n)

let string_of_float_reduced n =
  if (mod_float n 1.) != 0.
  then string_of_int (int_of_float n)
  else string_of_float n

let operation_string n1 n2 op op_string =
  let result = op n1 n2 in
  if (mod_float result 1.) != 0.
    then string_of_float_reduced n1 ^ op_string ^ string_of_float_reduced n2
    else string_of_float_reduced result

let rec fact n =
  if n = 0 then 1 else
  if n = 1 then 1 else
  n * fact (n-1)

let rec derivative_sin n = 
  if n > 0 && (n mod 2) != 0 then cos else derivative_cos 1
and derivative_cos n = 
  if n > 0 && (n mod 2) != 0
  then (fun x -> -. sin x)
  else (fun x -> -. (derivative_sin 1) x)

let taylor_series f g a n =
  let x = 
    if a = 0.
    then "x"
    else "(x " ^ abs_string a ^ ")"
  in
  let rec _aux acc f' p = 
    if p > n then acc
    else
      let derivative = g p in
      let derivative_value = derivative a in
      let den = float (fact (p)) in
      let sign = 
        if derivative_value >= 0. then " + " else " - " in
      let increment = 
        if derivative_value = 0. then ""
        else
          sign ^
          operation_string 
            (abs_float derivative_value) den (fun x y -> x /. y) "/" ^ 
          x ^ "^" ^ string_of_int (p)
      in _aux (acc ^ increment) derivative (p+1)
  in
    if n >= 0 
    then _aux (string_of_float_reduced (f a)) f 1
    else failwith "a must be non-negative"

let sin_series = taylor_series sin derivative_sin
let cos_series = taylor_series cos derivative_cos