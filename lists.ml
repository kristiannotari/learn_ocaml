let list = [1; 2; 3; 4; 5];;

let rec map f l = match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec filter f l = match l with
  | [] -> []
  | hd::tl -> if f hd then hd::(filter f tl) else filter f tl

let rec reduce acc f l = match l with
  | [] -> acc
  | hd::tl -> reduce (f hd acc) f tl

let rec zip_till_shortest l1 l2 = match (l1, l2) with
  | ([], []) | (_, []) | ([], _) -> []
  | (hd1::tl1, hd2::tl2) -> (hd1, hd2)::(zip_till_shortest tl1 tl2)

let rec pairwise l = match l with
  | hd1::hd2::tl -> (hd1,hd2)::(pairwise (hd2::tl))
  | _ -> []

let enumerate l =
  let rec enumerate acc n = function
  hd :: tl -> enumerate ((n,hd)::acc) (n+1) tl | [] -> List.rev acc
  in enumerate [] 0 l