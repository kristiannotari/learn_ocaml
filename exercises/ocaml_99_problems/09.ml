(* 
 * DIFFICULTY: medium
 *
 * Pack consecutive duplicates of list elements into sublists
 *)

let pack list =
  let rec _pack acc tmp = function
    | [] -> acc
    | [e] -> (e::tmp)::acc
    | hd1::(hd2::_ as tl) -> 
      if hd1 = hd2
      then _pack acc (hd1::tmp) tl
      else _pack ((hd1::tmp)::acc) [] tl
  in 
  List.rev (_pack [] [] list);;