module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
      *)
    type 'a t = 'a -> 'a list

    (** La stratégie vide : ne renvoie aucune proposition de réduction *)
    val empty : 'a t

    (* TYPES DE BASE *)

    (** Stratégie de réduction sur les entiers
      * @param n entier
      * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
      *)
    val int : int t

    (** Stratégie de réduction sur les entiers positifs
      * @param n entier positif
      * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
      *)
    val int_nonneg : int t

    (** Stratégie de réduction sur les flottants
      * @param x flottant
      * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
      *)
    val float : float t

    (** Stratégie de réduction sur les flottants positifs
      * @param x flottant positif
      * @return  liste de flottants positifs plus "simples" entre `0` et `x`
      *)
    val float_nonneg : float t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères plus "simples"
      *)
    val char : char t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères minuscule et majuscule
      *)

      val char_casse : char t
    
    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t
    
     (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
      val alphanum_casse : char t

    (* CHAINES DE CARACTERES *)

    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t
    
    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
      
    val string_reduction_substring : char t -> string t

    (* LISTES *)

    (** Stratégie de réduction sur les listes
      * @param red stratégie de réduction à utiliser sur chaque élément
      * @param l   liste
      * @return    liste de listes plus "simples" au pire aussi longues que `l`
      *)
    val list : 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Stratégie de réduction sur les couples
      * @param fst_red stratégie de réduction de la première coordonnée
      * @param snd_red stratégie de réduction de la deuxième coordonnée
      * @return        stratégie de réduction sur les couples correspondants
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un filtre à une stratégie de réduction
      * @param p   filtre à appliquer à chaque réduction
      * @param red stratégie de réduction
      * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t
  end =
  struct
    type 'a t = 'a -> 'a list ;;

    (* Implementation de empty *)

    let empty : 'a t = fun _ -> []


    (** Version récursive terminale de Reduction.int
      * @param n entier
      * @param nb_values entier positif
      * @param liste_red liste d'entiers
      * @return liste_red avec nb_values*2 - 1 deux entiers plus simples que n 
      *)

    let rec int_rec ( n,  nb_values, liste_red ) = 
        if (n == 0 || nb_values <= 0 ) then 
            liste_red
        else 
            let new_n = (n * (nb_values - 1)) / nb_values in
                if(new_n ==0 ) then
                    int_rec ( new_n, nb_values - 1, new_n :: liste_red )
                else
                    int_rec ( new_n, nb_values - 1, new_n :: (- new_n) :: liste_red )


    (** Implémentation de Reduction.int*)

    let int : int t = fun n -> int_rec(n, 10, [])


    (** Version récursive terminale de Reduction.int_nonneg
      * @param n entier
      * @param nb_values entier positif
      * @param liste_red liste d'entiers
      * @return liste_red avec nb_values*2 - 1 deux entiers plus simples que n 
      *)

    let rec int_nonneg_rec ( n,  nb_values, liste_red ) = 
        if (n <= 0 || nb_values <= 0 ) then 
            liste_red
        else 
            let new_n = (n * (nb_values - 1)) / nb_values in
                int_nonneg_rec ( new_n, nb_values - 1, new_n :: liste_red )

    (** Implémentation de Reduction.int_nonneg*)

    let int_nonneg : int t = fun n -> int_nonneg_rec(n, 10, [])

    (** Version récursive terminale de Reduction.float
      * @param x flottant
      * @param min_val flottant
      * @param nb_values flottant positif
      * @param liste_red liste de flottants
      * @return liste_red avec nb_values*2 - 1 deux flottants plus simples que x
      *)

    let rec float_rec ( x, min_val,  nb_values, liste_red ) = 
        if (abs_float(x) < abs_float(min_val) || nb_values < 1.0 ) then 
            liste_red
        else 
            let new_x = (x *. (nb_values -. 1.)) /. nb_values in
                if(new_x == 0. ) then
                    float_rec ( new_x, min_val, nb_values -. 1., new_x :: liste_red )
                else
                    float_rec ( new_x, min_val, nb_values -. 1., new_x :: (-. new_x) :: liste_red )


    (** Implémentation de Reduction.float *)

    let float : float t = fun x -> float_rec(x, ((x/.10.)-.0.0001), 10., [])


    (** Version récursive terminale de Reduction.float
      * @param x flottant positif
      * @param nb_values flottant positif
      * @param liste_red liste de flottants positif
      * @return liste_red avec nb_values de flottants positif plus simples que x
      *)

    let rec float_nonneg_rec ( x, nb_values, liste_red ) = 
        if (x <= 0. || nb_values < 1.0 ) then 
            liste_red
        else 
            let new_x = (x *. (nb_values -. 1.)) /. nb_values in
                float_nonneg_rec ( new_x, nb_values -. 1., new_x :: liste_red )



    (** Implémentation de Reduction.float_nonneg *)

    let float_nonneg : float t = fun x -> float_nonneg_rec(x, 10., [])
    
     let char c =
    match c with
    | _ -> [c];;

  let char_casse c =
    [Char.lowercase_ascii c; Char.uppercase_ascii c]
    

  let alphanum c =
    match c with
    _ -> [c];;

  let alphanum_casse c =
    [Char.lowercase_ascii c; Char.uppercase_ascii c]

  let string char_reduction s =
    let rec reduce (acc : string list) (i : int) : string list =
      if i >= String.length s then List.rev acc
      else
        let c = s.[i] in
        let reduced_chars = char_reduction c in
        let reduced_strings = List.map (fun rc -> String.concat "" [String.sub s 0 i; String.make 1 rc; String.sub s (i+1) (String.length s - i - 1)]) reduced_chars in
        reduce (List.rev_append reduced_strings acc) (i+1)
    in
    reduce [s] 0
  
  let string_reduction_substring char_reduction s =
    let rec reduce (acc : string list) (len : int) (i : int) : string list =
      if len >= String.length s then List.rev acc
      else if i >= String.length s - len then reduce acc (len+1) 0
      else
        let substring = String.sub s i len in
        let reduced_substrings = List.map (fun rc -> String.concat "" [String.sub s 0 i; String.make 1 rc; String.sub s (i+len) (String.length s - i - len)]) (List.flatten (List.map char_reduction (String.to_seq substring |> List.of_seq))) in
        reduce (List.rev_append reduced_substrings acc) len (i+1)
    in
    reduce [s] 1 0
    
    let list element_reduction l =
      let rec reduce acc i =
        if i >= List.length l then List.rev acc
        else
          let reduced_elements = element_reduction (List.nth l i) in
          let reduced_lists =
            List.fold_left
              (fun acc' re -> List.rev_append (List.rev_map (fun acc'' -> re::acc'') acc') acc)
              [] reduced_elements
          in
          reduce reduced_lists (i+1)
      in
      reduce [l] 0
      
    

  let combine fst_red snd_red =
    fun (x, y) ->
      let fst_red_x = fst_red x in
      let snd_red_y = snd_red y in
      List.concat (List.map (fun a -> List.map (fun b -> (a, b)) snd_red_y) fst_red_x)
  

  let filter p red x =
    List.filter p (red x);;
    
  end ;;
let red_empty = Reduction.empty;;
let red_2_int = Reduction.int(2)
let red_2_int_nonneg = Reduction.int_nonneg(2)
let red_2_float = Reduction.float(2.)
let red_2_float_nonneg = Reduction.float_nonneg(2.)
let red_char = Reduction.char('a');;
let red_char_casse = Reduction.char_casse('A');;
let red_alphanum = Reduction.alphanum('a');;
let red_alphanum_casse = Reduction.alphanum_casse('A');;
let red_string = Reduction.string(Reduction.char) "abc";;
let red_string_reduction_substring = Reduction.string_reduction_substring(Reduction.char) "abc";;
let red_list = Reduction.list(Reduction.char) ['a'; 'b'; 'c'];;
let red_combine = Reduction.combine(Reduction.string(Reduction.char_casse)) (Reduction.string(Reduction.char)) ("abc", "def");;
let red_filter = Reduction.filter (fun x -> x = 'a') (Reduction.char_casse) 'A';;
