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

  end ;;
