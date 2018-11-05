(* 
 * DIFFICULTY: easy
 *
 * Run-length encoding of a list
 *)

let encode list =
  let rec _encode acc tmp = function
    | [] -> acc
    | [e] -> (tmp, e)::acc
    | hd1::(hd2::_ as tl) -> 
      if hd1 = hd2
      then _encode acc (tmp+1) tl
      else _encode ((tmp, hd1)::acc) 1 tl
  in 
  List.rev (_encode [] 1 list);;