(** Initialisation du générateur aléatoire **)
let _ = Random.self_init()

(** Taille du plateau **)
let n = 4

let noms_joueurs = [|"Horace"; "Vera"|]


(** Types utilisés **)
(* Configurations *)
type etat = int * int array * int array
(* Coups *)
type coup = int * int
(* Stratégies *)
type strategie = etat -> coup


(** Fonctions à identifier, question 2 **)

(* Les deux tableaux des positions sont le même mais ce n'est pas grave puisqu'on ne les utilisera que de manière persistante (et c'est pourquoi on fait une copie du tableau des positions avant de jouer un coup dans jouer. *)
let configuration_initiale :etat =
  let initial = Array.make n 0 in
  initial.(0) <- n-1;
  initial.(n-1) <- n-1;
  0, initial, initial

(* Permute les joueurs et leurs positions *)
let tour_suivant ((joueur, moi, lui):etat) :etat =
  (1 - joueur, lui, moi)

(* Indique si un état est final gagnant et si oui pour quel joueur *)
let terminal ((joueur, moi, lui):etat) :int option =
  if moi = Array.make n (n-1) then
    Some joueur
  else if lui = Array.make n (n-1) then
    Some (1-joueur)
  else
    None

(** Fonctions à identifer, question 3 **)

(* Donne la configuration qui suit un état selon un coup donné *)
let jouer ((joueur, moi, lui):etat) ((i, d):coup) :etat =
  let nouveau_moi = Array.copy moi in
  nouveau_moi.(i) <- moi.(i) + d;
  tour_suivant (joueur, nouveau_moi, lui)

(* Liste les coups licites pour un joueur dans une configuration. Il est inutile de savoir qui est le joueur puisque par convention le premier tableau de positions est celui du joueur qui joue *)
let coups_possibles ((_, moi, lui):etat) :coup list =
  let res = ref [] in
  for i = 1 to n - 2 do
    let j = moi.(i) in
    if j < n - 1 then
      if lui.(j + 1) != i then
        res := (i, 1) :: !res
      else if lui.(j + 1) = i && lui.(j + 2) != i then
        res := (i, 2) :: !res   
  done;
  if !res = [] then [(1, 0)] else !res


(** Fonction d'affichage **)
let affiche ((joueur, moi, lui):etat) :unit =

  let hpos, vpos =
    match joueur with
    | 0 -> moi, lui
    | 1 -> lui, moi
    | _ -> failwith "Le joueur doit être 0 ou 1"
  in

  print_string "    ";
  for i = 1 to n - 2 do
    Printf.printf "%i " i
  done;
  print_newline ();

  let p = Array.make_matrix n n ' ' in
  p.(0).(0) <- 'x';
  p.(0).(n - 1) <- 'x';
  p.(n - 1).(0) <- 'x';
  p.(n - 1).(n - 1) <- 'x';
  for i = 1 to n - 2 do
    p.(i).(hpos.(i)) <- '>';
    p.(vpos.(i)).(i) <- 'v';
  done;

  for i = 0 to n - 1 do
    if i = 0 || i = n - 1 then print_char ' ' else print_int i;
    print_char ' ';
    for j = 0 to n - 1 do
      Printf.printf "%c " p.(i).(j);
    done;
    print_newline ()
  done;
  print_newline ()


(** Fonction de partie **)
(* De manière transparente, s0 est la stratégie du joueur 0 donc de Horace et s1 celle de Vera. Le booléen affichage permet d'afficher ou non la partie, cad les configurations visitées *)
let partie (etat:etat) (s0:strategie) (s1:strategie) (affichage:bool) :int =
  let rec config_suivante (etat:etat) (s0:strategie) (s1:strategie) :int =
    if affichage then affiche etat;
    let i, direction = s0 etat in
    let joueur, _, _ = etat in
    if affichage then begin
        if direction = 0 then
          Printf.printf "%s passe.\n\n" noms_joueurs.(joueur)
        else
          Printf.printf "%s joue en %d.\n\n" noms_joueurs.(joueur) i
    end;
    let etat = jouer etat (i,direction) in
    match terminal etat with
    | Some joueur ->
       if affichage then begin
         affiche etat;
         Printf.printf "%s a gagné !\n" noms_joueurs.(joueur)
       end;
       joueur
    | None -> config_suivante etat s1 s0 (* on inverse l'ordre des stratégies pour faire jouer le joueur suivant *)
  in
  config_suivante etat s0 s1


(** Stratégie interactive **)

let lire_entier () :int =
  try read_int ()
  with Failure _ -> 0

let coup_interactif (etat:etat) :coup =
  let j, _, _ = etat in
  let coups = coups_possibles etat in
  if coups = [(1, 0)] then begin
    Printf.printf "%s : vous ne pouvez pas jouer. Passe.\n" noms_joueurs.(j);
    1, 0
  end else begin
    Printf.printf "%s : quel coup voulez-vous jouer (indice de ligne ou de colonne) ? " noms_joueurs.(j);
    let i = ref (lire_entier ()) in
    while not (1 <= !i && !i <= n - 2 && List.mem_assoc !i coups) do
      Printf.printf "Veuillez entrer un coup valide : ";
      i := lire_entier ()
    done;
    !i, List.assoc !i coups
  end

(* let _ = partie configuration_initiale coup_interactif coup_interactif true *)

(** Question 6 **)
let coup_aleatoire (etat:etat) :coup =
  let coups = coups_possibles etat in
  let n = List.length coups in
  let i = Random.int n in
  let rec trouver_coup_i (l:coup list) (i:int) :coup = match l with
    |[] -> failwith "i n'est pas un numéro de coup valide"
    |t::q -> if i = 0 then t else trouver_coup_i q (i-1)
  in trouver_coup_i coups i

(** Question 7 **)
(* Compte le nombre de parties gagnées par chacun des deux joueurs lorsqu'ils suivent les stratégies s0 et s1 respectivement *)
let simuler_parties (etat:etat) (s0:strategie) (s1:strategie) (nb_parties:int) :int array =
  let nb_gagnes = Array.make 2 0 in
  for i = 1 to nb_parties do
    let g = partie etat s0 s1 false in
    nb_gagnes.(g) <- 1 + nb_gagnes.(g)
  done;
  nb_gagnes

(* Avantage pour Vera sur taille 3, avantage pour Horace sur tailles 4,5 *)
let _ = simuler_parties configuration_initiale coup_aleatoire coup_aleatoire 1000000

(** Question 8 : min-max simple **)
let rec min_max (etat:etat) :int =
  match terminal etat with
  |Some 0 -> max_int
  |Some 1 -> min_int
  |None -> let coups = coups_possibles etat in
           let etats_suivants = List.map (jouer etat) coups in
           let scores = List.map min_max etats_suivants in
           let joueur, _, _ = etat in
           if joueur = 0 then List.fold_left max min_int scores
           else List.fold_left min max_int scores
  |_ -> failwith "il n'y a que deux joueurs"

let _ = min_max configuration_initiale


(* Renvoie le score optimal atteignable par le joueur et le coup qui le donne étant donnée la liste de coups possibles et les scores et coups optimaux (ceux ci ne sont pas utiles) de ces derniers dans le même ordre, calculés récirsivement via min-max *)
let rec coup_opti (joueur:int) (coups:coup list) (scores:(int*coup) list) :int*coup =
  let etage = [|max;min|] in
  match coups, scores with 
  |[c], [s,_] -> s,c
  |c::q1, (s,_)::q2 -> let score_fin, coup_fin = coup_opti joueur q1 q2 in
                       if etage.(joueur) s score_fin = s then s,c else score_fin, coup_fin
  |_ -> failwith "ne devrait pas arriver"

(* Modification de la fonction min-max pour qu'elle donne le coup permettant d'obtenir le score de l'état *)
let rec min_max_coup (etat:etat) :int*coup =
  match terminal etat with
  |Some 0 -> max_int, (-1,-1) (* coup inutile *)
  |Some 1 -> min_int, (-1,-1) (* coup inutile *)
  |None -> let coups = coups_possibles etat in
           let etats_suivants = List.map (jouer etat) coups in
           let scores = List.map min_max_coup etats_suivants in
           let joueur, _, _ = etat in
           coup_opti joueur coups scores
  |_ -> failwith "il n'y a que deux joueurs"

(** Question 10 **)
let coup_min_max (etat:etat) :coup =
  let s,c = min_max_coup etat in c

let _ = simuler_parties configuration_initiale coup_min_max coup_aleatoire 1000

(** Question 11 **)
(* Faire jouer optimalement les deux joueurs résulte en la victoire d'Horace puisqu'on a vu avec min_max que la configuration initiale est gagnante pour lui, donc même en jouant parfaitement Vera ne peut pas gagner *)
let _ = partie configuration_initiale coup_min_max coup_min_max false

(* Faire jouer aléatoirement Horace et optimalement Vera fait que cette fois il devient possible à cette dernière de gagner *)
let _ = simuler_parties configuration_initiale coup_aleatoire coup_min_max 10000

(** Question 12 **)
(* On réexploite la fonction simuler_parties. On cherche une stratégie pour Horace donc on veut une heuristique positive pour les positions où Horace est gagnant, d'où le sens de la différence *)
let heuristique (etat:etat) (k:int) :int =
  let stats = simuler_parties etat coup_aleatoire coup_aleatoire k in
  stats.(0) - stats.(1)

(** Question 13 **)
let coup_min_max_borne (etat:etat) (p:int) (k:int) :coup =
  let rec min_max_borne (etat:etat) (p:int) :int*coup =
    match terminal etat with
    |Some 0 -> max_int, (-1,-1)
    |Some 1 -> min_int, (-1,-1)
    |None when p = 0 -> (heuristique etat k), (-1, -1)
    |None ->
      let coups = coups_possibles etat in
      let etats_suivants = List.map (jouer etat) coups in
      let scores = List.map (fun e -> min_max_borne e (p-1)) etats_suivants in
      let joueur, _, _ = etat in
      coup_opti joueur coups scores
    |Some _ -> failwith "n'arrive pas, il n'y a que deux joueurs"
  in let _, c = min_max_borne etat p in c

(** Question 14 **)

(* Aléatoire VS aléatoire : ratio de victoire 80% - 20% *)
let _ = simuler_parties configuration_initiale coup_aleatoire coup_aleatoire 10000
(* Min-max borné VS min-max optimal : ratio de victoire 100% - 0%, même avec des faibles profondeurs *)
let _ = simuler_parties configuration_initiale (fun e -> coup_min_max_borne e 1 1) coup_min_max 100
(* Aléatoire VS min-max optimal : ratio de victoire 60% - 40%, cohérent *)
let _ = simuler_parties configuration_initiale coup_aleatoire coup_min_max 10000
(* Aléatoire VS min-max borné à profondeur 1 : ratio de victoire 70% - 30%, cohérent *)
let _ = simuler_parties configuration_initiale coup_aleatoire (fun e -> coup_min_max_borne e 1 1) 10000
(* Augmenter la profondeur aide Vera à gagner mais même sans ça il suffit d'augmenter le nb de parties jouées pour calculer l'heuristique pour retrouver des résultats similaires au min-max optimal *)
let _ = simuler_parties configuration_initiale coup_aleatoire (fun e -> coup_min_max_borne e 3 1) 10000
(* Aléatoire VS min-max à profondeur 1 et 5 parties : 60% - 40% donc comme avec le min-max opti ! *)
let _ = simuler_parties configuration_initiale coup_aleatoire (fun e -> coup_min_max_borne e 1 5) 10000


(* A partir d'ici : faire avec n = 10 *)
(* Aléatoire VS aléatoire : ratio de victoire 56% - 44% *)
let _ = simuler_parties configuration_initiale coup_aleatoire coup_aleatoire 10000
(* Aléatoire VS min_max à profondeur 1, 1 partie : ratio de victoire 18 - 82 *)
let _ = simuler_parties configuration_initiale coup_aleatoire (fun e -> coup_min_max_borne e 1 1) 1000
(* Aléatoire VS min_max à profondeur 1, 10 partie : ratio de victoire 3 - 97 *)
let _ = simuler_parties configuration_initiale coup_aleatoire (fun e -> coup_min_max_borne e 1 5) 100
(* Aléatoire VS min_max à profondeur 2, 1 partie : ratio de victoire 8 - 92 *)
let _ = simuler_parties configuration_initiale coup_aleatoire (fun e -> coup_min_max_borne e 2 1) 100
(* Même avec faible profondeur et peu de parties, le joueur désavantagé devient ultra favorisé face à la strat aléatoire *)
let _ = simuler_parties configuration_initiale (fun e -> coup_min_max_borne e 1 1) (fun e -> coup_min_max_borne e 1 1) 1000
