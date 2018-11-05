(* 
 * DIFFICULTY: easy
 *
 * Find the last but one (last and penultimate) elements of a list
 *)

let rec last_two = function
  | [] | [_] -> None
  | [e1; e2] -> Some (e1, e2)
  | hd::tl -> last_two tl;;