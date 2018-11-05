(* 
 * DIFFICULTY: easy
 *
 * Find the number of elements of a list
 *)

let length list = 
  let rec _count acc l = match l with
    | [] -> acc
    | hd::tl -> _count (acc+1) tl
  in
  _count 0 list;;