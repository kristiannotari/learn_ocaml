(* 
 * EXERCISE 1
 * A few of Chemistry
 *
 * Put into a list, called alkaline_earth_metals, the atomic numbers of the six 
 * alkaline earth metals: beryllium (4), magnesium (12), calcium (20), strontium
 * (38), barium (56), and radium (88). Then
 *
 * 1) Write a function that returns the highest atomic number in
 * alkaline_earth_metals.
 * 2) Write a function that sorts alkaline_earth_metals in ascending order (from
 * the lightest to the heaviest).
 * 
 * Put into a second list, called noble_gases, the noble gases: helium (2), neon
 * (10), argon (18), krypton (36), xenon (54), and radon (86). Then
 *
 * 3) Write a function (or a group of functions) that merges the two lists and
 * print the result as couples (name, atomic number) sorted in ascending order
 * on the element names.
 *)

let alkaline_earth_metals = [
  ("beryllium", 4);
  ("magnesium", 12);
  ("calcium", 20);
  ("strontium", 38);
  ("barium", 56); 
  ("radium", 88);
];;

(* 1 *)
let highest_atomic_number l = 
  let rec highest h l = match l with
    | [] -> h
    | (_, an)::tl -> if an > h then highest an tl else highest h tl 
  in highest (-1) l;;

(* 2 *)
let sort_asc_by_atomic_number l = 
  let rec qsort = function
    | [] -> []
    | ((_, an) as e)::tl -> 
        (qsort (List.filter (fun (_, x) -> (x <= an)) tl))
        @ [e] @
        (qsort (List.filter (fun (_, x) -> (x > an)) tl))
  in qsort l;;

let noble_gases = [
  ("helium", 2);
  ("neon", 10);
  ("argon", 18);
  ("krypton", 36);
  ("xenon", 54);
  ("radon", 86);
];;

(* 3 *)
let merge_and_sort_asc_by_atomic_number l1 l2 =
  sort_asc_by_atomic_number (l1 @ l2);;