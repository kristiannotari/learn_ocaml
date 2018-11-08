module type IntervalI = sig
  type interval
  type endpoint
  val create : endpoint -> endpoint -> interval
  val is_empty : interval -> bool
  val contains : interval -> endpoint -> bool
  val intersect : interval -> interval -> interval
  val tostring : interval -> string
  exception WrongInterval
end;;
module type Comparable = sig 
  type t
  val compare : t -> t -> int
  val tostring : t -> string
end ;;

module MakeInterval(M: Comparable) : 
  (IntervalI with type endpoint = M.t) = struct
  exception WrongInterval
  type endpoint = M.t
  type interval =
    | Empty
    | Interval of endpoint * endpoint
  let create e1 e2 =
    if compare e1 e2 > 0
    then raise WrongInterval
    else Interval (e1, e2)
  let is_empty = function
    | Empty -> true
    | _ -> false
  let contains i e = match i with
    | Interval (e1, e2) ->
      compare e1 e >= 0 && compare e2 e < 0
    | Empty -> false
  let intersect i1 i2 = match i1, i2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (e1a, e1b), Interval (e2a, e2b) ->
      if compare e1b e2a >= 0 then
        Interval (
          (if compare e1a e2a >= 0 then e1a else e2a),
          (if compare e1b e2b >= 0 then e2b else e1b)
        )
      else Empty
  let tostring = function 
    | Interval (e1, e2) -> "[" ^ M.tostring e1 ^ ", " ^ M.tostring e2 ^ "]"
    | Empty -> "[]"
end;;

module IntInterval = MakeInterval(
  struct
    type t = int
    let compare t1 t2 = t1 - t2
    let tostring t1 = string_of_int t1
  end
  )

module StringInterval = MakeInterval(
  struct
    include String;;
    let tostring s = s
  end
  )

let main () =
  let i1 = IntInterval.create 3 8 and
  i2 = IntInterval.create 4 10 and
  s1 = StringInterval.create "abacus" "zyxt" and s2 = StringInterval.create "dog" "wax"
  in
  Printf.printf "%s\n" (IntInterval.tostring (IntInterval.intersect i1 i2)); try
    Printf.printf "%s\n"
      (StringInterval.tostring (StringInterval.create "wax" "fog"))
  with StringInterval.WrongInterval->Printf.printf "Exception: WrongInterval\n"; Printf.printf "%s\n"
      (StringInterval.tostring (StringInterval.intersect s1 s2)) ; Printf.printf
      "Does \"%s\" belong to %s? %B\nDoes it belong to %s? %B\n" "asylum" (StringInterval.tostring s2) (StringInterval.contains s2 "asylum") (StringInterval.tostring s1) (StringInterval.contains s1 "asylum") ;;
let() = main() ;;