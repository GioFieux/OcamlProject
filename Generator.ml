module Generator :
  sig
    (** Type du générateur pseudo-aléatoire de données de type 'a *)
    type 'a t

    (** Renvoie une nouvelle valeur aléeatoire
      * @param gen générateur pseudo-aléatoire
      * @return    nouvelle valeur aléatoire en utilisant `gen`
      *)
    val next : 'a t -> 'a

    (** Générateur constant
      * @param x valeur
      * @return  générateur de l'unique valeur `x`
      *)
    val const : 'a -> 'a t

    (* GENERATEURS DE TYPES DE BASE *)
 
    (** Générateur pseudo-aléatoire de booléens
      * @param prob probabilité de la valeur `true`
      * @return     générateur pseudo-aléatoire de valeurs booléennes
      *)
    val bool : float -> bool t

    (** Générateur pseudo-aléatoire d'entiers
      * @param a borne inférieure
      * @param b borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre `a` et `b` inclus
      *)
    val int : int -> int -> int   t

    (** Générateur pseudo-aléatoire d'entiers positifs ou nuls
      * @param n borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre 0 et `n` inclus
      *)
    val int_nonneg : int -> int   t

    (** Générateur pseudo-aléatoire de flottants
      * @param x borne supérieure
      * @param y borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre `x` et `y` inclus
      *)
    val float : float -> float -> float t

    (** Générateur pseudo-aléatoire de flottants positifs ou nuls
      * @param x borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre 0 et `x` inclus
      *)
    val float_nonneg : float -> float t

    (** Générateur pseudo-aléatoire de caractères *)
    val char : char t

    (** Générateur pseudo-aléatoire de caractères alphanumériques *)
    val alphanum : char t

    (* GENERATEURS DE CHAINE DE CARACTERE *)

    (** Générateur de chaînes de caractères
      * @param n   longueur maximale de la chaîne de caractère
      * @param gen générateur pseudo-aléatoire de caractères
      * @return    générateur pseudo-aléatoire de chaînes de caractères dont chaque caractéré est généré avec `gen`
      *)
    val string : int -> char t -> string t

    (* GENERATEURS DE LISTES *)

    (** Générateur de listes
      * @param n   longueur maximale de la liste
      * @param gen générateur pseudo-aléatoire d'éléments
      * @return    générateur pseudo-aléatoire de listes dont chaque élément est généré avec `gen`
      *)
    val list : int -> 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Générateur pseudo-aléatoire de couples
      * @param fst_gen générateur pseudo-aléatoire de la première coordonnée
      * @param snd_gen générateur pseudo-aléatoire de la deuxième coordonnée
      * @return        générateur pseudo-aléatoire du couple
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un post-traitement à un générateur pseudo-aléatoire
      * @param f   post-traitement à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `f` à chaque valeur générée par `gen`
      *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Applique un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire ne générant des valeurs de `gen` que si elles vérifient `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** Applique un post-traitement dépendant d'un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param f   couple des post-traitements à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `fst f` pour toute valeur vérifiant `p`
      *                                                          et `snd f` pour toute valeur ne le vérifiant pas
      *)
    val partitioned_map : ('a -> bool) -> (('a -> 'b) * ('a -> 'b)) -> 'a t -> 'b t
  end =
  struct
    (* TODO : Implémenter le type et tous les éléments de la signature *)
    
    type 'a t = {seed : int; gen : (int->'a)}
    		
    		
        let float x y = 
        	{seed = Random.int 1000; 
        	gen = (fun s -> Random.init s;
        		((Random.float (y-.x)) +. x)
        	)};;
    
    	let bool prob =
    		{seed = Random.int 1000;
    		gen = (fun s ->
    			Random.init s;
    			if (Random.float 1.) < prob
    			then
    				false
    			else
    				true
    		)}
    		;;

    	let int a b = 
    		{seed = Random.int 1000;
    		gen = (fun s -> 
    			Random.init s;
    			((Random.int (b-a)) + a)
    		)};;
    	
    	let const x = {seed = 0; gen = (fun _ -> x)};;
    	
    	let next gen = 
    		let s = Random.int 1000 in
    			gen.gen s
    		;;
    	
    	let int_nonneg = int 0;;
    	
    	let float_nonneg = float 0.;;
    	
    	let char = 
    		{seed = Random.int 1000; 
    		gen = fun s -> (
    			Random.init s;
    			Char.chr ((Random.int (126-33)) + 33)
    		)};;
    	
    	let alphanum = 
    		{seed = Random.int 1000; 
    		gen = fun s -> (
	    		match ((int 0 62).gen s) with
	    		| x when x < 10 -> Char.chr ((Random.int (57-48)) + 48)
	    		| x when x < 36 -> Char.chr ((Random.int (90-65)) + 65)
	    		| _ -> Char.chr ((Random.int (122-97)) + 97)
    		)};;
    	
    	let string n gen =
  		{ seed = Random.int 1000;
    		  gen = fun s -> (
    		  	Random.init s;
    		  	String.init n (fun x -> gen.gen (x+s)))};;
    	
    	let list n gen = 
    		{seed = Random.int 1000;
    		gen = fun s -> (
    			Random.init s;
    			let l = List.init n (fun x -> gen.gen (x+s)) in l
    		)};;
    	
    	let combine fst_gen snd_gen = 
    		{seed = Random.int 1000;
    		gen = fun s -> (fst_gen.gen s, snd_gen.gen s)
    		};;
    	
    	let map f gen = 
    		{seed = Random.int 1000 ;
    		gen = fun s -> f (gen.gen s)
    		};;
    		
    	
    	(* 	/!\
    	Si aucune valeur ne correspond au filtre, retourner quoi ? 
    		/!\	*)	
    	let rec secur_recurs stop next param n = 
		if (n<0 || stop param)
		then
			param
		else
			secur_recurs stop next (next param) (n-1)
		;;
    		
    	
    	let filter p gen = 
    		{seed = Random.int 1000;
    		gen = fun s -> (
    			let x = (gen.gen s) in
	    			if p x
	    			then
    					x
    				else 
    					gen.gen (secur_recurs (fun param -> p (gen.gen param)) (fun param -> Random.int 1000) 1000 1000)
    		)};;
		
	
	let partitioned_map p f gen = 
    		{seed = Random.int 1000;
    		gen = fun s -> (
    			Random.init s;
    			let x = gen.gen s in
    				if p x
    				then
    					(fst f) x
    				else
    					(snd f) x
    		)};;
    
  end ;;
