  sig
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

  type 'a t = 'a -> 'a list
  
  let empty _ = [];;

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
let red_char = Reduction.char('a');;
let red_char_casse = Reduction.char_casse('A');;

let red_alphanum = Reduction.alphanum('a');;

let red_alphanum_casse = Reduction.alphanum_casse('A');;

let red_string = Reduction.string(Reduction.char) "abc";;

let red_string_reduction_substring = Reduction.string_reduction_substring(Reduction.char) "abc";;

let red_list = Reduction.list(Reduction.char) ['a'; 'b'; 'c'];;

let red_combine = Reduction.combine(Reduction.string(Reduction.char_casse)) (Reduction.string(Reduction.char)) ("abc", "def");;


let red_filter = Reduction.filter (fun x -> x = 'a') (Reduction.char_casse) 'A';;