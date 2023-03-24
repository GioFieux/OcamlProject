#use "Test.ml" ;;

(*Test de la fonctionnalité supplémentaire combine du module Property*)
(* Définir les propriétés *)
(*
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
*)


(* ---------------------------------------------- *)


(* Test Supplémentaire*)
(*
let abs_property x = x >= 0.;;
let abs_property_bis x = x<0.;;
let abs_test = Test.make_test
    (Generator.float (-20.) 20.)
    (Reduction.float)
    (abs_property);;

let abs_test_bis = Test.make_test
    (Generator.float (-20.) 20.)
    (Reduction.float)
    (abs_property_bis);;

let result = Test.check 1000 abs_test;;
let fail= Test.fails_at 1000 abs_test;;
let execute = Test.execute 1000 [abs_test; abs_test_bis];;

Printf.printf "Test result: %b\n" result;;
*)


(* ---------------------------------------------- *)


(* Tests de la division euclidienne                                                                          *)
(* Les tests sont effectués sur des couples d'entiers compris entre -100 et 100 dont le second est *non nul* *)
(* (d'où l'utilisation du filtre pour éviter des divisions par zéro).                                        *)
let gen_intcouple =
  let gen_dividend =                            Generator.int (-100) 100
  and gen_divisor  = Generator.filter ((<>) 0) (Generator.int (-100) 100)
    in Generator.combine gen_dividend gen_divisor ;;
let red_intcouple =
  let red_dividend =                           Reduction.int
  and red_divisor  = Reduction.filter ((<>) 0) Reduction.int
    in Reduction.combine red_dividend red_divisor ;;
let test_intcouple = Test.make_test gen_intcouple red_intcouple ;;

(* Construction des tests *)
(*let test_quorem = test_intcouple "/ et mod (correct)" (fun (a, b) -> (a = (a / b) * b + (a mod b))) ;;*)
let test_quorem = test_intcouple  (fun (a, b) -> (a = (a / b) * b + (a mod b))) ;;
(*let test_quorem_wrong = test_intcouple "/ et mod (faux)"   (fun (a, b) -> (a = (a / b) * b - (a mod b))) ;;*)
let test_quorem_wrong = test_intcouple    (fun (a, b) -> (a = (a / b) * b - (a mod b))) ;;

(* Exécution des tests *)
Test.check    100 test_quorem       ;;
Test.check    100 test_quorem_wrong ;;
Test.fails_at 100 test_quorem       ;;
Test.fails_at 100 test_quorem_wrong ;;
Test.execute  100 [test_quorem ; test_quorem_wrong] ;;


(* ---------------------------------------------- *)


(* Tests sur la concaténation de listes                                         *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlistcouple =
  let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) in
    Generator.combine gen_intlist gen_intlist ;;
let red_intlistcouple =
  let red_intlist = Reduction.list     Reduction.int_nonneg     in
    Reduction.combine red_intlist red_intlist ;;
let test_intlistcouple = Test.make_test gen_intlistcouple red_intlistcouple ;;

(* Constructon des tests *)
(*let test_append = test_intlistcouple "List.@ (correct)"  (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) + (List.length l2)) ;;*)
let test_append = test_intlistcouple (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) + (List.length l2)) ;;
(*let test_append_wrong = test_intlistcouple "List.@ (faux)" (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) - (List.length l2)) ;;*)
let test_append_wrong = test_intlistcouple (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) - (List.length l2)) ;;

(* Exécution des tests *)
Test.check    100 test_append       ;;
Test.check    100 test_append_wrong ;;
Test.fails_at 100 test_append       ;;
Test.fails_at 100 test_append_wrong ;;
Test.execute  100 [test_append ; test_append_wrong] ;;
