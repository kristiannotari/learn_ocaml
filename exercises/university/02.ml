(* 
 * EXERCISE 2
 * Temperature Conversion System
 *
 * Beyond the well-known Celsius and Fahrenheit, there are other six temperature
 * scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer (Look at
 * http://en.wikipedia.org/wiki/Comparison_of_temperature_scales to read about
 * them).
 *
 * 1) Write a function that given a pure number prints a conversion table for it
 * among any of the 8 scales (remember that functions are high-order).
 * 2) Write a function that given a temperature in a specified scale returns a
 * list of all the corresponding temperatures in the other scales, note that the
 * scale must be specified (hint: use a tuple). 
 *)

let celsius_to_celsius    = fun x -> ("celsius", x +. 0.0);;
let celsius_to_fahrenheit = fun x -> ("fahrenheit", (x *. 1.8) +. 32.0);;
let celsius_to_kelvin     = fun x -> ("kelvin", x +. 273.15);;
let celsius_to_rankine    = fun x -> ("rankine", (x *. 1.8) +. 491.67);;
let celsius_to_delisle    = fun x -> ("delisle", (x *. -1.5) +. 150.0);;
let celsius_to_newton     = fun x -> ("newton", x *. 0.33);;
let celsius_to_reaumur    = fun x -> ("reaumur", x *. 0.8);;
let celsius_to_romer      = fun x -> ("romer", (x *. 0.525) +. 7.5);;

(* 1 *)
let rec conversion_table (x: float) f = match f with
  | [] -> ()
  | hd::tl ->
    let (name, value) = hd x in 
      (Printf.printf "%s: %f\n" name value); 
      conversion_table x tl;;

conversion_table 0.0 [
  celsius_to_celsius;
  celsius_to_delisle;
  celsius_to_newton;
  celsius_to_fahrenheit;
  celsius_to_rankine;
  celsius_to_reaumur;
  celsius_to_romer;
];;
conversion_table 10.0 [
  celsius_to_celsius;
  celsius_to_delisle;
  celsius_to_newton;
  celsius_to_fahrenheit;
  celsius_to_rankine;
  celsius_to_reaumur;
  celsius_to_romer;
];;

(* 2 *)
let conversion_functions = [
  ("celsius", celsius_to_celsius);
  ("fahreneit", celsius_to_fahrenheit);
  ("kelvin", celsius_to_rankine);
  ("rankine", celsius_to_rankine);
  ("delisle", celsius_to_delisle);
  ("newton", celsius_to_newton);
  ("reaumur", celsius_to_reaumur);
  ("romer", celsius_to_romer);
];;

let to_convert = [
  ("celsius", 0.0);
  ("fahreneit", 0.0);
  ("celsius", 10.0);
  ("fahreneit", 10.0);
]

let rec conversion_table_given_value_and_scale ((name: string), (value: float)) f = match f with
  | [] -> ()
  | (f_name, f_fun)::tl ->
    let (r_name, r_value) = 
      if name != f_name
      then (f_fun value)
      else ("original", value)
    in 
      (Printf.printf "%s: %f\t\t %s %s\n" r_name r_value name f_name); 
      conversion_table_given_value_and_scale (name, value) tl;;

let rec convert_values = function
  | [] -> ()
  | hd::tl -> 
    conversion_table_given_value_and_scale hd conversion_functions;
    convert_values tl ;;

convert_values to_convert;;