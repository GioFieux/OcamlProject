module Property :
  sig
    (** Type d'une propriété portant sur des éléments de type 'a
      * Une propriété est une fonction booléenne. *)
    type 'a t = 'a -> bool

    (* CONSTANTES *)

    (** Propriété toujours vérifiée *)
    val always_true  : 'a t

    (** Propriété jamais   vérifiée *)
    val always_false : 'a t
  end =
  struct
    type 'a t = 'a -> bool ;;

    (* TODO : Implémenter tous les éléments de la signature manquants *)
    (* always_true renvoie toujours true quelle que soit l'entrée *) 
    let always_true : 'a t = fun _ -> true
    
    (* always_false renvoie toujours false quelle que soit l'entrée. *)
    let always_false : 'a t = fun _ -> false
    
    (* Fonction qui combine deux propriétés en une nouvelle propriété qui est vraie si les deux propriétés sont vraies. *)
    let combine (p1 : 'a t) (p2 : 'a t) : 'a t = fun x -> p1 x && p2 x
   
  end;;
  
(** 
Supposons que nous avons une fonction sum qui calcule la somme des éléments d'une liste d'entiers. 
Nous voulons vérifier que sum renvoie toujours un entier positif ou nul. 
Voici comment cela peut être fait avec le module Property : 
**)

let prop_sum_positive : int list Property.t =
  fun lst -> sum lst >= 0

(**
Ici, nous avons créé une propriété prop_sum_positive qui est vraie si la somme des éléments de la liste est positive ou nulle.
**)

let () =
  let prop = Property.combine Property.always_true prop_sum_positive in
  Test.check prop

(**
Nous avons ensuite combiné cette propriété avec la propriété toujours vraie Property.always_true en utilisant la fonction Property.combine. 
Cela garantit que la propriété combinée est toujours vraie. 
Nous avons finalement vérifié cette propriété en utilisant la fonction Test.check.
**)

(* Fonction sum *)
let rec sum l =
  match l with
    []-> 0
  |h::t-> h + (sum t);;
