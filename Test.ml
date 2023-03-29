#use "Property.ml" ;;
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
    
    (* Fonctionnalité supplémentaire *)
    (** Génère un rapport détaillé des tests effectués
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      un rapport détaillé des tests réussis et échoués ainsi que des statistiques
      *)
    val generate_report : int -> ('a t) list -> string
  end =
  struct
    type 'a t = {
      gen: 'a Generator.t;
      red: 'a Reduction.t;
      prop: 'a Property.t;
    }
    
    (* Crée un nouveau test avec le générateur, la stratégie de réduction et la propriété donnés *)
    let make_test gen red prop = {
      gen = gen;
      red = red;
      prop = prop;
    }

    (* Teste récursivement si les 'n' éléments générés par 'gen' vérifient la propriété 'prop' *)
    let rec test_iter prop gen n =
      if n = 0 then
        true
      else
        let x = Generator.next gen in
        prop x && test_iter prop gen (n - 1)

    (* Vérifie si les 'n' éléments générés pour un test donné vérifient la propriété *)
    let check n test =
      test_iter test.prop test.gen n

    (* Trouve récursivement la première valeur générée par 'gen' qui ne vérifie pas la propriété 'prop' *)
    let rec find_fail prop gen red n =
      if n = 0 then
        None
      else
        let x = Generator.next gen in
        if not (prop x) then
          Some (x)
        else
          find_fail prop gen red (n - 1);;

    (* Trouve la première valeur qui ne vérifie pas la propriété pour un test donné *)
    let fails_at n test =
      find_fail test.prop test.gen test.red n

    (* Exécute une liste de tests et renvoie les résultats sous forme de liste de paires (test, valeur échouée) *)
    let execute n tests =
      List.map (fun test -> (test, fails_at n test)) tests
     
    (* Fonctionnalité supplémentaire *)
    (* Génère un rapport détaillé des tests effectués *)
    let generate_report n tests =
      let results = execute n tests in
      let total_tests = List.length tests in
      let failed_tests = List.filter (fun (_, fail) -> fail <> None) results in
      let failed_count = List.length failed_tests in
      let success_count = total_tests - failed_count in
      
      let report_header = "Test Report\n" ^ String.make 40 '-' ^ "\n" in
      let report_success = Printf.sprintf "Tests réussis: %d\n" success_count in
      let report_failed = Printf.sprintf "Tests échoués: %d\n" failed_count in
      let report_stats = Printf.sprintf "Total des tests: %d\n" total_tests in
      
      let report_failed_details = 
        failed_tests
        |> List.mapi (fun i (test, fail) -> Printf.sprintf "%d. Test échoué:\nPropriété: %s\nValeur: %s\n\n" (i + 1) (Property.to_string test.prop) (match fail with Some v -> Reduction.to_string test.red v | None -> "N/A"))
        |> String.concat "" in
      
      report_header ^ report_success ^ report_failed ^ report_stats ^ "\nDétails des tests échoués:\n" ^ report_failed_details
  end ;;
