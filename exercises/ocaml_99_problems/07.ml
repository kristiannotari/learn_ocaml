(* 
 * DIFFICULTY: medium
 *
 * Flatten a nested list structure
 *)

type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten list_of_nodes =
  let rec _flatten acc l = match l with
    | [] -> acc
    | (One e)::tl -> _flatten (acc @ [e]) tl
    | (Many e)::tl -> _flatten acc (e @ tl)
  in
  _flatten [] list_of_nodes;;