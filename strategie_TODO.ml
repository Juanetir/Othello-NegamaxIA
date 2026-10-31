open Type
open Unix 

type strategie = othello -> int
(*COMPILER

ocamlc unix.cma type.ml strategie_TODO.ml

*)

let cases x y = x*8 + y 
let index2cases idx = (idx/8, idx mod 8)
let string_of_case (x, y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"


let start_config :othello = 
  let start_plateau = Array.make 64 0 in
  start_plateau.(cases 3 3) <- 2;
  start_plateau.(cases 4 4) <- 2;
  start_plateau.(cases 3 4) <- 1;
  start_plateau.(cases 4 3) <- 1;
  {plateau = start_plateau;
  joueur = 1 (*NOIR*)}
  

let directions = [| -8; 8; -1; 1; -9; -7; 7; 9 |] (*chaque direction où on peut manger une pièce*)

let in_plateau (position:int) :bool = position < 64 && position >= 0
let is_valid_direction pos dir :bool = 
  let y = pos mod 8 in
  match dir with
  | -1 | -9 | 7 -> y > 0  (*Coté Gauche*)
  | 1 | -7 | 9 -> y < 7   (* Coté droit*)
  | -8 | 8 -> true        (* Haut et bas *)
  | _ -> false 

  
let legal_moves (p:othello) :int list =
  let moves = ref [] in
  let rival = 3 - p.joueur in
  for i = 0 to 63 do
    if p.plateau.(i) = 0 then
      let valid = ref false in 
      for d = 0 to Array.length directions - 1 do
        let dir = directions.(d) in
        let position = ref (i + dir) in
        let found_rival = ref false in
        while is_valid_direction (!position-dir) dir && in_plateau !position && p.plateau.(!position) = rival do
          found_rival := true;
          position := !position + dir
        done;
        if !found_rival && in_plateau !position && p.plateau.(!position) = p.joueur then
          valid := true
      done;
      if !valid then 
        moves := i :: !moves
    done;
    !moves


let game_over (p:othello) : bool = (*On regarde si l'un de joueurs peut poser une pièce*)
  let moves_current = legal_moves p in
  let rival = 3 - p.joueur in
  let p_rival = { p with joueur = rival } in
  let moves_rival = legal_moves p_rival in
  moves_current = [] && moves_rival = [] 

let chips_count (p:othello) :int = (*gagne noir >0; gagne blanc < 0*)
  let n = ref 0 in
  Array.iter 
  (fun x ->
    if x = 1 then
      n := !n + 1
    else if x = 2 then 
      n := !n - 1 
      ) p.plateau; 
  !n



let print_othello (p:othello) : unit =
  Printf.printf "\n     0 1 2 3 4 5 6 7\n";
  Printf.printf "   -------------------\n";
  
  for i = 0 to 7 do
    Printf.printf " %d |" i;
    for j = 0 to 7 do
      let idx = i * 8 + j in
      let symbol =
        match p.plateau.(idx) with
        | 0 -> "."
        | 1 -> "N"   (* Noir *)
        | 2 -> "B"   (* Blanc *)
        | _ -> "?"
      in
      Printf.printf " %s" symbol
    done;
    Printf.printf " |\n"
  done;
  
  Printf.printf "   -----------------\n";
  Printf.printf "Tours du joueur: %s\n\n" (if p.joueur = 1 then "NOIR (1)" else "BLANC (2)")

let print_othello (p:othello) : unit =
  let count_black = ref 0 in
  let count_white = ref 0 in
  let possible_moves = legal_moves p in
  Printf.printf "\n     0 1 2 3 4 5 6 7\n";
  Printf.printf "   -------------------\n";
  
  for i = 0 to 7 do
    Printf.printf " %d |" i;
    for j = 0 to 7 do
      let idx = cases i j in
      match p.plateau.(idx) with
      | _ when List.mem idx possible_moves ->   Printf.printf " □"
      | 0 ->
          Printf.printf " ."
      | 1 ->
          incr count_black;
          Printf.printf " \027[91m●\027[0m"
      | 2 ->
          incr count_white;
          Printf.printf " \027[1m●\027[0m"
      | _ ->
          Printf.printf " ?"
    done;
    Printf.printf " |\n"
  done;
  
  Printf.printf "   -------------------\n";
  Printf.printf "\027[91mNOIRS\027[0m: %d   \027[1mBlanc\027[0m: %d\n"
    !count_black !count_white;
  Printf.printf "Tours du joueur: %s\n\n" (if p.joueur = 1 then "\027[91mNOIRS\027[0m: (1)" else "\027[1mBLANCS\027[0m: (2)")


let apply_move (p:othello) (idx:int) : unit = (**On Suppose le mouvement valide*)
  let rival = 3 - p.joueur in
  p.plateau.(idx) <- p.joueur; 

  Array.iter (fun dir ->
    let to_flip = ref [] in
    let position = ref (idx + dir) in
    
    while in_plateau !position && is_valid_direction (!position-dir) dir &&
          p.plateau.(!position) = rival do
      to_flip := !position :: !to_flip;
      position := !position + dir
    done;
    if in_plateau !position && p.plateau.(!position) = p.joueur && !to_flip <> [] then
      List.iter (fun pos -> p.plateau.(pos) <- p.joueur) !to_flip
  ) directions;

  p.joueur <- 3 - p.joueur




let coup_interactif (p:othello) : int =
  let moves = legal_moves p in
  if moves = [] then
    -1
  else begin
    (*SHOW POSSIBLES*)
    Printf.printf "Mouvements possibles:\n";
    List.iter (fun m ->
      let (x,y) = index2cases m in
      Printf.printf " %d -> (%d,%d)\n" m x y
    ) moves;    
    (* LIRE le choix*)
    let rec ask () =
      Printf.printf "mettre l'indice: ";
      let input = read_line () in
      try
        let choice = int_of_string input in
        if List.mem choice moves then choice
        else (Printf.printf "Choisi un numero dans la liste.\n"; ask ())
      with _ ->
        Printf.printf "Il faut quand même utiliser des números.\n";
        ask ()
    in
    ask ()
  end


let chips_number_self (p:othello) :int*int =
  let n_self = ref 0 in
  let n_rival = ref 0 in
  Array.iter
    (fun x ->
      if x = p.joueur then
        n_self := !n_self + 1
      else if x = 3 - p.joueur then
        n_rival := !n_rival + 1
  ) p.plateau;
  (!n_self, !n_rival)

  let weights = [| (*Les poids ont été pris sans trop de logique théorique, mais pratique*)
      300; -20; 15; 10; 10; 15; -20; 300;
      -20; -50; -10; 15; 15; -10; -50; -20;
      15; -10; 12; 12; 12; 10; -10; 15;
      10; 15; 12; 0; 0; 12; 15; 10;
      10; 15; 12; 0; 0; 12; 15; 10;
      15; -10; 12; 12; 12; 12; -10; 15;
      -20; -50; -10; 15; 15; -10; -50; -20;
      300; -20; 15; 10; 10; 15; -20; 300
    |] 

let stable_chips (p:othello) (joueur:int) : int = (*C'est pas vraiment de la stabilité*)
  let stable = Array.make 64 false in

  let mark_from_corner start_idx delta_row delta_col =
    let rec loop idx row col =
      if row < 0 || row > 7 || col < 0 || col > 7 then () else
      let i = row * 8 + col in
      if p.plateau.(i) = joueur then begin
        stable.(i) <- true;
        loop (i + delta_row*8 + delta_col) (row + delta_row) (col + delta_col)
      end
    in
    let start_row = start_idx / 8 in
    let start_col = start_idx mod 8 in
    loop start_idx start_row start_col
  in

  mark_from_corner 0 0 1;   
  mark_from_corner 0 1 0;   
  mark_from_corner 7 0 (-1); 
  mark_from_corner 7 1 0;   
  mark_from_corner 56 (-1) 0;
  mark_from_corner 56 0 1;  
  mark_from_corner 63 (-1) 0;
  mark_from_corner 63 0 (-1); 
  Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 stable


let heuristique (p:othello) (my_moves_list:int list): int =
  let w =
    Array.mapi (fun i x -> match p.plateau.(i) with
      | j when j = p.joueur -> x
      | rival when rival = 3 - p.joueur -> -x
      | _ -> 0
    ) weights |> Array.fold_left (+) 0 
  in
  let c = 
    let (cnf, cnr) = chips_number_self p in
      cnf - cnr
  in
  let m = 
    let my_moves = List.length my_moves_list in
    let rival_moves = List.length (legal_moves {p with joueur = 3 - p.joueur}) in
    my_moves - rival_moves
  in
  let s = (stable_chips p p.joueur) - (stable_chips p (3 - p.joueur)) 
  in
  8*w + 6*c + 45*m + 20*s


let copy_board (p:othello) = {plateau = Array.copy p.plateau; joueur = p.joueur} 

let score_move p move =
  let pos_weight = weights.(move) in
  let corner_bonus =
    if move = 0 || move = 7 || move = 56 || move = 63 then 10000
    else 0
  in
  corner_bonus + pos_weight

  (**
  j'ai trouvé ce lien dans un forum, StackOverflow
  https://github.com/kartikkukreja/blog-codes/blob/master/src/Heuristic%20Function%20for%20Reversi%20(Othello).cpp
  
  Pour Negamax j'ai cherché sur Wikipedia
  *)

let coup_IA (board:othello) :int = 
  (* let start_time = Unix.gettimeofday () in *)
  let rec negamax (p:othello) (depth:int) (alpha:int) (beta:int) : int*int =
    let coups_possibles = legal_moves p
    |> List.sort (fun m1 m2 ->
        compare (score_move p m2) (score_move p m1))
      in
    if depth = 0 || game_over p then
      ((heuristique p coups_possibles), -1) (*Heuristique en POV du joueur*)
    else
      if coups_possibles = [] then
        let rival_p = {p with joueur = 3 - p.joueur} in
        let scores,_ = negamax rival_p (depth-1) (-beta) (- alpha) in
        (-scores, -1)
      else
        let rec loop moves best_score best_move alpha =
          match moves with
          | [] -> (best_score, best_move)
          | move :: rest ->
              let child = copy_board p in
              apply_move child move;
              let (child_score, _) =
                negamax child (depth-1) (-beta) (-alpha)
              in

              let score = -child_score in

              let new_best_score, new_best_move =
                if score > best_score then
                  (score, move)
                else
                  (best_score, best_move)
              in

              let new_alpha = max alpha score in
              if new_alpha >= beta then
                (new_best_score, new_best_move)   (* beta cut *)
              else
                loop rest new_best_score new_best_move new_alpha
        in

      loop coups_possibles min_int (-1) alpha
  in
  let (_, coup) = negamax board 8 min_int max_int in
  (* let end_time = Unix.gettimeofday () in
  let _ = Printf.printf ">\tTime %0.1f\n" (end_time -. start_time) in *)
  coup
    

let coup_aleatoire (p:othello) : int =
  Random.self_init();
  let coups = legal_moves p in
  if coups = [] then
    -1 
  else
    let idx = Random.int (List.length coups) in
    List.nth coups idx

let partie (p:othello) (s1:strategie) (s2:strategie) (affichage:bool) :int =
  let rec config_suivante (p:othello) (s1:strategie) (s2:strategie) :int =
    let index = s1 p in
    if affichage then print_othello p;
    let legal_list = legal_moves p in
    if legal_list = [] then
      begin
      Printf.printf "%d passe.\n\n" p.joueur;
      config_suivante {plateau = p.plateau; joueur = 3 - p.joueur} s2 s1
      end
    else
      begin
      Printf.printf "%d joue en %d.\n\n%!" p.joueur index;
      apply_move p index;
      match game_over p with
      | false -> config_suivante p s2 s1
      | true -> if affichage then begin
        print_othello p;
        match chips_count p with
        | 0 -> Printf.printf "Personne n'a gagné\n"
        | noir when noir > 0 -> Printf.printf "Noir (1) a gagné🎉\n"
        | blanc (*when blanc > 0*) -> Printf.printf "blanc (2) a gagné🎉\n"
        end;
        p.joueur
      end 
      in 
      config_suivante p s1 s2

let _ = Printf.printf "\n\n\n\n"

let _ = partie start_config coup_IA coup_IA true
