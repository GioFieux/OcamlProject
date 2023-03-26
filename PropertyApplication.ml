#use "Test.ml" ;;

(*Test de la fonctionnalité supplémentaire combine du module Property*)
(* Définir les propriétés *)

let positive_sum_prop : (int * int) Property.t = fun (x, y) -> x + y > 0
let even_sum_prop : (int * int) Property.t = fun (x, y) -> (x + y) mod 2 = 0
(* Combiner les propriétés *)
let combined_prop = Property.combine positive_sum_prop even_sum_prop
(* Créer un générateur de paires d'entiers *)
let int_gen = Generator.int (-100) 100
let pair_gen = Generator.combine int_gen int_gen
(* Définir une stratégie de réduction 
let int_red = Reduction.int
let pair_red = Reduction.combine int_red int_red *)
(* Créer un test *)
let test = Test.make_test pair_gen pair_red combined_prop
(* Effectuer le test *)
let n = 1  (* Nombre de valeurs à tester *)
let result = Test.check n test
(* Afficher le résultat *)
let () = if result
         then Printf.printf "Le test a réussi.\n"
         else Printf.printf "Le test a échoué.\n" 
