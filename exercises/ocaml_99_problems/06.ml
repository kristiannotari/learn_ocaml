(* 
 * DIFFICULTY: easy
 *
 * Find out whether a list is a palindrome
 *)

let is_palindrome list = 
  let r_list = List.rev list in
  let rec _check l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | hd1::tl1, hd2::tl2 ->
      if hd1 = hd2 then _check tl1 tl2 else false
  in 
  _check list r_list;;