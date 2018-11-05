(* currying *)
let abs_diff =
  (fun x -> (fun y -> abs (x - y)));;

let abs_diff_2 x y =
  abs (x - y);;

(* partial application *)
let abs_diff_of_3 = abs_diff 3;;

let abs_diff_2_of_3 y = abs_diff_2 3 y;;

(* partial application without currying *)
let many_arguments a b c d e f = 
  a;b;c;d;e;f;;

let many_arguments_caller = many_arguments 3;;