(* 
 * DIFFICULTY: easy
 *
 * Reverse a list
 *)

let reverse list = 
  let rec _reverse acc l = match l with
    | [] -> acc
    | hd::tl -> _reverse (hd::acc) tl
  in
  _reverse [] list;;