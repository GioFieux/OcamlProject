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
    (* TODO : Implémenter le type et tous les éléments de la signature *)
    type 'a t = {
      name: string;
      property: 'a Property.t;
      generator: 'a Generator.t;
      reduction: 'a Reduction.t;
    }

    let create name property generator reduction = { name; property; generator; reduction }

    let test ?(count=100) ?(size=100) test =
      let rec aux n =
        if n = 0 then Ok () else
        let param = Generator.generate ~size:size test.generator in
        match test.property param with
        | true -> aux (n-1)
        | false -> (
          match Reduction.reduce ~count:count ~size:size param test.reduction with
          | [] -> Error (param, None)
          | counterexample::_ -> Error (param, Some counterexample)
      )
      in aux count

    let tests ?(count=100) ?(size=100) tests =
      List.fold_left (fun acc test ->
        match test ?count:count ~size:size with
       | Ok () -> acc
       | Error (param, None) -> (test.name, param, None)::acc
       | Error (param, Some counterexample) -> (test.name, param, Some counterexample)::acc
      ) [] tests
  end ;;
