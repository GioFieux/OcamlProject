#use "Test.ml" ;;

(*Test de la fonctionnalité supplémentaire combine du module Test*)

(* Propriété 1 : Vérifie si un nombre est pair *)
let is_even x = x mod 2 = 0;;

(* Propriété 2 : Vérifie si un nombre est impair *)
let is_odd x = x mod 2 <> 0;;

(* Générateur de nombres entiers entre -100 et 100 *)
let int_gen = Generator.int (-100) 100;;

(* Réduction pour les nombres entiers *)
let int_red = Reduction.int;;

(* Création des tests *)
let test_even = Test.make_test int_gen int_red (Property.from_predicate is_even);;
let test_odd = Test.make_test int_gen int_red (Property.from_predicate is_odd);;

(* Exécution des tests *)
let test_results = Test.execute 1000 [test_even; test_odd];;

(* Génération du rapport *)
let report = generate_test_report test_results;;
print_endline report;;
