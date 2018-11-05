(* 
 * EXERCISE 5
 * Playing around with strings
 *
 * Define the following functions/operators on strings:
 * 1) is_palindrome: string → bool that checks if the string is palindrome, a
 * string is palindrome when the represented sentence can be read the same way
 * in either directions in spite of spaces, punctual and letter cases, e.g.,
 * detartrated, "Do geese see God?", "Rise to vote, sir.", ...
 *
 * 2) operator (-): string → string → string that subtracts the letters in a string
 * from the letters in another string, e.g., "Walter Cazzola"-"abcwxyz" will
 * give "Wlter Col" note that the operator - is case sensitive
 *
 * 3) anagram : string → string list → boolean that given a dictionary of
 * strings, checks if the input string is an anagram of one or more of the
 * strings in the dictionary
 *)

let get_words_list = 
  let root = "resources" in
  let file = "dictionary.txt" in
  let path = root ^ "/" ^ file in
  let ic = open_in path in 
  let rec loop acc = 
    try 
      let word = input_line ic in
      loop (acc @ [word])
    with e ->
      close_in_noerr ic;
      acc
  in loop []
let is_word_char = function
  | (('A' .. 'Z') as c) | (('a' .. 'z') as c) -> true
  | _ -> false

let list_of_string ?inc_special:(i_s = false) ?to_lowercase:(t_l = true) string = 
  let rec _aux i l = 
    if i < 0
    then l
    else begin 
      let char = string.[i] in 
      if (is_word_char char) || i_s
      then 
        if t_l
        then _aux (i-1) ((Char.lowercase_ascii char)::l)
        else _aux (i-1) (char::l)
      else _aux (i-1) (l)
    end
  in _aux ((String.length string) - 1) []

let string_of_list list = 
  let rec _aux acc = function 
    | [] -> acc
    | hd::tl -> _aux (acc ^ (String.make 1 hd)) tl

  in _aux "" list

let reverse string = 
  let list = list_of_string string in
  List.rev list

let seek list e =
  let rec _aux = function
    | [] -> false
    | hd::tl ->
      if e = hd
      then true
      else _aux tl
  in _aux list

let seek_and_remove list e =
  let rec _aux acc = function
    | [] -> (false, acc)
    | hd::tl ->
      if e = hd
      then (true, acc @ tl)
      else _aux (acc @ [hd]) tl
  in _aux [] list

let has_content = function
 | Some x -> true
 | None -> false

let analyze_anagram string word =
  let list_string = list_of_string string ~inc_special:true ~to_lowercase:false in 
  let list_word = list_of_string word ~inc_special:true ~to_lowercase:false in 
  let rec _aux l1 l2 = match l1,l2 with
    | [],[] -> true
    | [],_ | _,[] -> false
    | hd1::tl1, l2 -> 
      let (result, rem) = seek_and_remove l2 hd1 in
      if result
      then _aux tl1 rem
      else false
  in 
    if _aux list_string list_word
    then Some word
    else None

let content = function
  | Some x -> x
  | None -> ""
(* 1 *)
let is_palindrome string = 
  let reversed = reverse string in
  let rec _aux l1 l2 = match l1,l2 with
    | [], [] -> true
    | [],_ | _,[] -> false
    | hd1::tl1, hd2::tl2 -> 
      if hd1 = hd2
      then _aux tl1 tl2
      else false
  in 
  _aux (list_of_string string) reversed

(* 2 *)
let ( - ) s1 s2 = 
  let list_s1 = list_of_string s1 ~inc_special:true ~to_lowercase:false in 
  let list_s2 = list_of_string s2 ~inc_special:true ~to_lowercase:false in 
  let rec _aux acc l1 l2 = match l1,l2 with
    | [],[] | [],_ | _,[] -> acc
    | hd1::tl1, l2 -> 
      if seek l2 hd1
      then _aux acc tl1 l2
      else _aux (acc @ [hd1]) tl1 l2
  in
  string_of_list (_aux [] list_s1 list_s2)

(* 3 *)
let anagram string string_list = 
  let rec _aux = function
    | [] -> false
    | hd::tl -> 
      let result = analyze_anagram string hd in
      if has_content result
      then true
      else _aux tl
  in _aux string_list