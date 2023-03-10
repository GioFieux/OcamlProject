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
