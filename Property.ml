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
    
    (* Fonctionnalité supplémentaire *)
    val combine : 'a t -> 'a t -> 'a t
  end =
  struct
    type 'a t = 'a -> bool ;;

    (* TODO : Implémenter tous les éléments de la signature manquants *)
    (* always_true renvoie toujours true quelle que soit l'entrée *) 
    let always_true : 'a t = fun _ -> true
    
    (* always_false renvoie toujours false quelle que soit l'entrée. *)
    let always_false : 'a t = fun _ -> false
    
    (* Fonctionnalité supplémentaire *)
    (* Fonction qui combine deux propriétés en une nouvelle propriété qui est vraie si les deux propriétés sont vraies. *)
    let combine (p1 : 'a t) (p2 : 'a t) : 'a t = fun x -> p1 x && p2 x
   
  end;;
  
