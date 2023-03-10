module Reduction :
  sig
    type 'a t = 'a -> 'a list

    val empty : 'a t

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

    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t

    (* CHAINES DE CARACTERES *)

    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t

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


    let alphanum c =
        match c with
            _ -> [c];;
  
  
    let alphanum_lower c =
        match c with
            | 'A' .. 'Z' -> [Char.chr (Char.code c + 32)]
            | _ -> [c];;

    let rec string (red: char -> char list) (s: string) =
        match s with
            | "" -> []
            | _ ->
        let n = String.length s in
        let rec helper i =
            if i >= n then []
            else
                let char_reductions = red s.[i] in
                let substrings = List.map (fun c -> String.make 1 c) char_reductions in
                    let rest_substrings = helper (i + 1) in
                        List.concat (List.map (fun s -> List.map (fun rest -> s ^ rest) rest_substrings) substrings)
                            in
                                helper 0;;
          

      

    let list (red: 'a -> 'a list) (s: 'a list) =
        let rec helper s =
            match s with
                | [] -> [[]]
                | c::s' ->
                    let char_reductions = red c in
                        let substrings = List.map (fun c' -> [c']) char_reductions in
                            let rest_substrings = helper s' in
                                List.concat (List.map (fun s -> List.map (fun rest -> s @ rest) rest_substrings) substrings)
                                    in
                                        helper s;;
      
  

    let combine red1 red2 (x, y) =
        let l1 = red1 x in
            let l2 = red2 y in
                List.map (fun x -> (x, y)) l1 @ List.map (fun y -> (x, y)) l2;;


    let filter p red x =
        List.filter p (red x);;

  end ;;

let red_2_int = Reduction.int(2)
let red_2_int_nonneg = Reduction.int_nonneg(2)
let red_2_float = Reduction.float(2.)
let red_2_float_nonneg = Reduction.float_nonneg(2.)