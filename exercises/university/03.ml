(* 
 * EXERCISE 3
 * Matrix calculi
 *
 * Write the matrix datatype with the following operations:
 * 1) A function zeroes to construct a matrix of size n×m filled with zeros.
 * 2) A function identity to construct the identity matrix (the one with all 0s
 * but the 1s on the diagonal) of given size.
 * 3) A function init to construct a square matrix of a given size n filled with
 * the first n×n integers.
 * 4) A function transpose that transposes a generic matrix independently of its
 * size and content.
 * 5) A function * that multiplies two matrices non necessarily square matrices.
 *)

(* matrix datatype *)
type 'a matrix =
  | Matrix of ('a list) list

(* matrix filler (helper function) *)
let rec matrix_filler rows cols e =
  let rec _aux acc acc_r r c e = 
    if r > 0 then
      if c > 0
      then _aux acc ((e r c)::acc_r) r (c-1) e
      else _aux (acc_r::acc) [] (r-1) cols e
    else acc
  in
  _aux [] [] rows cols e;;

(* 1 *)
let zeroes n m = 
  matrix_filler n m (fun row col -> 0)

(* 2 *)
let identity n m =
  matrix_filler n m (fun row col -> if row = col then 1 else 0)

(* 3 *)
let init n =
  matrix_filler n n (fun row col -> (n * row) - (n - col));;

(* 4 *)    
let transpose m = 
  let rec _firsts acc_firsts acc_rest = function
    | []::_ | [] -> (acc_firsts, acc_rest)
    | (hd::tl)::row -> _firsts (acc_firsts @ [hd]) (acc_rest @ [tl]) row
  in
  let rec _aux acc = function
    | []::_ | [] -> acc
    | to_transpose -> 
      let
        (firsts, rest) = _firsts [] [] to_transpose
      in 
      _aux (acc @ [firsts]) rest
  in
  _aux [] m

(* 5 *)
let ( * ) m1 m2 =
  let m1_cols = List.length (transpose m1) in
  let m2_rows = List.length m2 in
  if (m1_cols != m2_rows) then None
  else begin
    let m2_t = transpose m2 in
    let m2_cols = List.length m2_t in
    let repeats = (m2_cols) - 1 in 
    let rec _mult_row_col acc row col = match row,col with
    | [],_ | _,[] -> acc
    | hd1::tl1, hd2::tl2 -> _mult_row_col (acc + (hd1 * hd2)) tl1 tl2
    in
    let rec _aux acc acc_row m1 m2 rem_repeats = match m1,m2 with
      | [],_ | _,[] -> acc
      | (row::tl1 as m1_tmp), col::tl2 -> 
        let
          new_acc_row = acc_row @ [_mult_row_col 0 row col]
        in
        if rem_repeats = 0 then
          _aux (acc @ [new_acc_row]) [] tl1 m2_t repeats
        else 
          _aux acc new_acc_row m1_tmp tl2 (rem_repeats-1)
    in
    Some (_aux [] [] m1 m2_t repeats)
  end