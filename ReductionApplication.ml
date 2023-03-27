#use "Reduction.ml";;

(** Exemples d'utilisation **)

let red_empty = Reduction.empty;;

let red_2_int = Reduction.int(2);;

let red_2_int_nonneg = Reduction.int_nonneg(2);;

let red_2_float = Reduction.float(2.);;

let red_2_float_nonneg = Reduction.float_nonneg(2.);;

let red_char = Reduction.char('a');;

let red_char_casse = Reduction.char_casse('A');;

let red_alphanum = Reduction.alphanum('a');;

let red_alphanum_casse = Reduction.alphanum_casse('A');;

let red_string = Reduction.string(Reduction.char) "abc";;

let red_string_reduction_substring = Reduction.string_reduction_substring(Reduction.char) "abc";;

let red_list = Reduction.list(Reduction.char) ['a'; 'b'; 'c'];;

let red_combine = Reduction.combine(Reduction.string(Reduction.char_casse)) (Reduction.string(Reduction.char)) ("abc", "def");;

let red_filter = Reduction.filter (fun x -> x = 'a') (Reduction.char_casse) 'A';;