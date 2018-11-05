(* 
 * DIFFICULTY: easy
 *
 * Find the k'th element of a list
 *)

let rec at n l = match l with
  | [] -> None
  | hd::tl -> 
    if n <= 0 then None
    else if n == 1 then Some hd
    else at (n-1) tl