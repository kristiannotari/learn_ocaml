(* BASIC *)

module type X_int = sig
  val x : int
end;;

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end;;

module Zero : X_int = struct
  let x = 0
end;;

module One : X_int = Increment(Zero);;

(* INTERMEDIATE *)

module type Comparable = sig 
  type t
  val compare : t -> t -> int
end;;

 module type Interval_intf = sig
   type t
   type endpoint
   val create : endpoint -> endpoint -> t
   val is_empty : t -> bool
   val contains : t -> endpoint -> bool
   val intersect : t -> t -> t
  end;;

module Make_interval(Endpoint : Comparable) : 
  (Interval_intf with type endpoint := Endpoint.t) = struct
    
    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    (** [create low high] creates a new interval from [low] to
        [high].  If [low > high], then the interval is empty *)
    let create low high =
      if Endpoint.compare low high > 0 then Empty
      else Interval (low,high)

    (** Returns true iff the interval is empty *)
    let is_empty = function
      | Empty -> true
      | Interval _ -> false

    (** [contains t x] returns true iff [x] is contained in the
        interval [t] *)
    let contains t x =
      match t with
      | Empty -> false
      | Interval (l,h) ->
        Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

    (** [intersect t1 t2] returns the intersection of the two input
        intervals *)
    let intersect t1 t2 =
      let min x y = if Endpoint.compare x y <= 0 then x else y in
      let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (l1,h1), Interval (l2,h2) ->
        create (max l1 l2) (min h1 h2)

  end ;;

module Int_interval = Make_interval(
    struct
      type t = int
      let compare t1 t2 =
        if t1 > t2 then 1
        else if t1 = t2 then 0
        else -1 
    end
  );;

module String_interval = Make_interval(String);;

Int_interval.create 4 6