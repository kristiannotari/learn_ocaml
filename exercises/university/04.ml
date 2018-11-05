(* 
 * EXERCISE 4
 * Frequencies
 *
 * Let's write a function (or a pool of functions) that given a quite large text
 * (over than 2000 words) counts how frequent each word occurs in the text.
 * The text is read from a file (look at the pervasive module in the manual) and
 * it is a real text with punctuation (i.e., commas, semicolons, ...) that
 * should be counted.
 *
 * Note that words with different case should be considered the same.
 *)

let root = "resources"
let file = "text_file_long.txt"
let path = root ^ "/" ^ file

let print_words words = 
  print_string "\n\nPRINT WORDS";
  let rec _aux pos = function
    | [] -> print_string ("\nEnd of words (total=" ^ (string_of_int pos) ^ ")\n")
    | (w,c)::tl -> 
      print_string ("\nword: " ^ w ^ "\ncount: " ^ (string_of_int c));
      _aux (pos+1) tl
  in _aux 0 words

let process_char = function
  | (('A' .. 'Z') as c) | (('a' .. 'z') as c) -> (true, String.make 1 c)
  | c -> (false, "\\" ^ (string_of_int (Char.code c)))

let is_in l word =
  let rec _aux pos = function
    | [] -> (false, -1)
    | (w, _)::tl -> 
      if word = w
      then (true, pos)
      else _aux (pos + 1) tl
  in  _aux 0 l

let rec add_count l pos = 
  let rec _aux acc_left pos = function
    | [] -> []
    | (w, c)::tl -> 
      if pos = 0
      then acc_left @ [w, c+1] @ tl
      else _aux (acc_left @ [(w, c)]) (pos-1) tl
  in _aux [] pos l

let add_word words = function
  | "" -> words
  | word ->
    let (result, pos) = is_in words word in
    if result
    then add_count words pos
    else words @ [(word, 1)]

let analyze_word completed words word char = 
  if completed
  then (words, word ^ char)
  else (add_word words word, char)

let () =
  let ic = open_in path in 
  let rec loop last (words, word) = 
    try 
      print_string "\nSTEP\n"; 
      print_string ("word (old): " ^ word ^ "\n");
      let char = input_char ic in 
      let (is_word_char, char) = process_char char in
      print_string char;
      print_string (": " ^ string_of_bool is_word_char);
      loop is_word_char (analyze_word (last && is_word_char) words word char);
    with e ->
      close_in_noerr ic;
      words
  in 
  print_words (loop false ([], ""));
  flush stdout;
  close_in ic;