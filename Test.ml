#use  "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :
  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t

    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> 'a Property.t -> 'a t

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool

    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val fails_at : int -> 'a t -> 'a option

    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list
  end =
  struct
    type 'a t = 'a Generator.t * 'a Reduction.t * 'a Property.t
    let make_test gen red prop = (gen, red, prop)

    let check n (gen, red, prop) =
      let rec loop i =
        if i >= n then true
        else
          let input = Generator.next gen in
          let simplified_input = Reduction.simplify red input prop in
          if not (Property.combine prop simplified_input) then false
          else loop (i + 1)
      in
      if n > 0 then loop 0 else true

    let fails_at n (gen, red, prop) =
      let rec loop i =
        if i >= n then None
        else
          let input = Generator.next gen in
          let simplified_input = Reduction.simplify red input prop in
          if not (Property.combine prop simplified_input) then Some simplified_input
          else loop (i + 1)
      in
      if n > 0 then loop 0 else None

    let execute n tests =
      List.map (fun test -> (test, fails_at n test)) tests
  end ;;
  
  
let test1 =
  let prop = Property.always_true in
  let gen = Generator.int_nonneg 100 in
  let red = Reduction.no_reduction in
  Test.make_test gen red prop

let result1 = Test.check 100 test1
(* result1 doit être true *)

let test2 =
  let prop = Property.always_false in
  let gen = Generator.int_nonneg 100 in
  let red = Reduction.no_reduction in
  Test.make_test gen red prop

let result2 = Test.check 100 test2
(* result2 doit être true *)

let test3 =
  let prop x = x mod 2 = 0 in
  let gen = Generator.int 1 100 in
  let red = Reduction.no_reduction in
  Test.make_test gen red prop

let result3 = Test.fails_at 100 test3
(* result3 doit être égal à Some (le premier entier impair généré par gen) *)

let prop1 x = x > 0
let test1 = Test.make_test (Generator.int 1 100) Reduction.no_reduction prop1

let prop2 x = x mod 2 = 0
let test2 = Test.make_test (Generator.int 1 100) Reduction.no_reduction prop2

let prop3 x = x mod 3 = 0
let test3 = Test.make_test (Generator.int 1 100) Reduction.no_reduction prop3

let result4 = Test.execute 100 [test1; test2; test3]
(* result4 est une liste de paires (test, result) pour chaque test effectué *)

