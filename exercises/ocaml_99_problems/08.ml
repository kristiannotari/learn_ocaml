(* 
 * DIFFICULTY: medium
 *
 * Eliminate consecutive duplicates of list elements
 *)

let rec compress = function
  | hd1::(hd2::_ as tl) -> 
    if hd1 = hd2
    then compress tl
    else hd1 :: (compress tl)
  | compressed -> compressed