#use "Generator.ml";;

(** Exemples d'utilisation**)
let gen_int = Generator.int 1 30;;
Generator.next gen_int;;

let gen_intnonneg = Generator.int_nonneg 50;;
Generator.next gen_intnonneg;;

let gen_string = Generator.string 8 Generator.char;;
Generator.next gen_string;;

let gen_char = Generator.char;;
Generator.next gen_char;;

let gen_list = Generator.list 8 Generator.char;;
Generator.next gen_list;;
