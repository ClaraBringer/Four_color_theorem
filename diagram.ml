open Graphics;;
open Voronoi;;
open Examples;;
open Unix;;

let grey = rgb 200 200 200;;

(* Inutile mais à garder pour les tests *)
let string_of_color_option c =
  let s = ref "" in
  begin
    match c with
    | Some(rgb) ->
      if rgb = red then s := "red"
      else if rgb = blue then s := "blue"
      else if rgb = green then s := "green"
      else if rgb = yellow then s := "yellow"
    | None -> s := "none"
  end;
  !s
;;

module Variables_Voronoi =
struct
  type t = int * color
  let compare (a, b) (c, d) =
    if a > c then 1
    else if a < c then -1
    else if b > d then 1
    else if b < d then -1
    else 0
end;;

module Solver = Sat_solver.Make (Variables_Voronoi);;

(* Couleurs du jeu *)
let colors = [|white; red; green; blue; yellow|];;

(* Construit une chaine de caractères à partir d'une dimension
   (i, j) : dimension
   return : chaine de caractères *)
let string_of_dim (i, j) =
  (string_of_int i)^("x")^(string_of_int j)
;;

(* Crée une distance
   q : entier
   return : une distance *)
let make_distance q =
  fun (x1, y1) (x2, y2) -> (float_of_int (abs (x1 - x2)))**q +. (float_of_int (abs (y1 - y2)))**q
;;

(* La distance euclidienne *)
let euclidean_distance =
  make_distance 2.
;;

(* La distance taxicab *)
let taxicab_distance =
  make_distance 1.
;;

(* Construit la matrice des régions du diagramme
   distance_function : une fonction de distance
   voronoi : un diagramme de Voronoi
   return : une matrice d'entiers *)
let regions_voronoi voronoi distance_function =
  let lines = snd voronoi.dim and
  columns = fst voronoi.dim and
  seeds_number = Array.length voronoi.seeds in
  let matrix = (Array.make_matrix columns lines (-1)) in
  for x = 0 to (columns - 1) do
    for y = 0 to (lines - 1) do
      let index = ref (-1) and
      min_dist = ref (distance_function (0, 0) (columns, lines)) in
      for k = 0 to (seeds_number - 1) do
        let dist = distance_function (x, y) (voronoi.seeds.(k).x, voronoi.seeds.(k).y) in
        if dist < !min_dist then
          begin
            min_dist := dist;
            index := k;
          end;
      done;
      matrix.(x).(y) <- !index;
    done;
  done;
  matrix
;;

(* Dessine les carrés de couleur *)
let draw_color_squares () =
  for i = 0 to 4 do
    let width = 100 and
    height = (size_y ()) / 6 in
    let side = ((min width height) - 4) in
    let middle = size_x () - 250 in
    let x = middle - side / 2 and
      y = (size_y () * i) / 6 + 2 in
    set_color black;
    draw_rect x y side side;
    set_color colors.(i);
    fill_rect (x + 1) (y + 1) (side - 2) (side - 2);
  done;
;;

(* Dessine un bouton
   x : abscisse du coin inférieur gauche du rectangle
   y : ordonnée du coin inférieur gauche du rectangle
   width : largeur du rectangle
   height : hauteur du rectangle
   message : message du bouton *)
let draw_button x y width height message =
  draw_rect x y width height;
  set_color grey;
  fill_rect (x + 1) (y + 1) (width - 2) (height - 2);
  moveto (x + width/3) (y + height/2);
  set_color black;
  draw_string message
;;

(* Dessine les boutons selon le moment du jeu
   game_state : le moment du jeu *)
let draw_buttons game_state =
  set_color black;
  let x = size_x () - 190 and
    width = 180 and
    height = size_y () / 6 in
  if game_state = 1 then
    let y = size_y () / 3 in
    draw_button x y width height "Solution"
  else if game_state = 2 then
    let y_play = size_y () / 6 and
    y_quit = size_y () / 2 in
    draw_button x y_play width height "Rejouer";
    draw_button x y_quit width height "Quitter"
;;

(* Dessine le diagramme
   voronoi : un diagramme de Voronoi
   matrix_regions : la matrice des régions associée à voronoi *)
let draw_voronoi voronoi matrix_regions game_state =
  let columns = Array.length matrix_regions and
  lines = Array.length matrix_regions.(0) in
  open_graph (" "^(string_of_int (columns + 300))^"x"^(string_of_int lines));
  auto_synchronize false;
  for x = 0 to (columns - 1) do
    for y = 0 to (lines - 1) do
      if (((x > 1) && (matrix_regions.(x).(y) <> matrix_regions.(x-1).(y)))
          || ((x < columns - 1) && (matrix_regions.(x).(y) <> matrix_regions.(x+1).(y)))
          || ((y > 1) && (matrix_regions.(x).(y) <> matrix_regions.(x).(y-1)))
          || ((y < lines - 1) && (matrix_regions.(x).(y) <> matrix_regions.(x).(y+1)))
          || x = columns - 1)
      then
        set_color black
      else
        begin
          match voronoi.seeds.(matrix_regions.(x).(y)).c with
          | None -> set_color white
          | Some(rgb) -> set_color rgb
        end;
      plot x y;
    done;
  done;
  draw_color_squares ();
  draw_buttons game_state;
  synchronize ()
;;

(* Construit la matrice d'adjacence des régions du diagramme
   voronoi : un diagramme de Voronoi
   matrix_regions : la matrice des régions associée à voronoi
   return : une matrice de booléens *)
let adjacences_voronoi voronoi matrix_regions =
  let seeds_number = Array.length voronoi.seeds and
  columns = Array.length matrix_regions and
  lines = Array.length matrix_regions.(0) in
  let adjacences = Array.make_matrix seeds_number seeds_number false in
  for h = 0 to (seeds_number - 1)  do
    for k = 0 to (seeds_number - 1) do
      if h <> k then
        begin
          for i = 0 to (columns - 1) do
            for j = 0 to (lines - 1) do
              if (matrix_regions.(i).(j) = h && (((i > 0) && matrix_regions.(i-1).(j) = k)
                                         || ((i < columns - 1) && matrix_regions.(i+1).(j) = k)
                                         || ((j > 0) && matrix_regions.(i).(j-1) = k)
                                         || ((j < lines - 1) && matrix_regions.(i).(j+1) = k)))
              then
                adjacences.(h).(k) <- true
            done;
          done;
        end;
    done;
  done;
  adjacences
;;

(* Construit le tableau des couleurs des régions
   voronoi : un diagramme de Voronoi
   return : un tableau de color option *)
let is_seed_colored voronoi =
  let seeds_number = Array.length voronoi.seeds in
  let colored_seeds = Array.make seeds_number None in
  for i = 0 to (seeds_number - 1) do
    match voronoi.seeds.(i).c with
    | None -> colored_seeds.(i) <- None
    | Some(rgb) -> colored_seeds.(i) <- Some(rgb)
  done;
  colored_seeds
;;

(* Vérifie s'il existe une couleur à une région
   colored_seeds : un tableau de couleur des régions
   return : une liste de clauses *)
let exists colored_seeds =
  let seeds_number = Array.length colored_seeds and
  exist_array = ref [] in
  for i = 0 to (seeds_number - 1) do
    let clause = ref [] in
    for c = 1 to 4 do
      clause := (true, (i, colors.(c)))::(!clause)
    done;
    exist_array := (!clause)::(!exist_array)
  done;
  !exist_array
;;

(* Vérifie que chaque région a au plus une couleur
   colored_seeds : un tableau de couleur des régions
   return : une liste de clauses *)
let unique colored_seeds =
  let seeds_number = Array.length colored_seeds and
  unique_array = ref [] in
  for i = 0 to (seeds_number - 1) do
    for c = 1 to 4 do
      for c' = 1 to 4 do
        if c <> c' then
          unique_array := [(false, (i, colors.(c))); (false, (i, colors.(c')))]::(!unique_array)
      done;
    done;
  done;
  !unique_array
;;

(* Vérifie que deux régions adjacentes n'ont pas la même couleur
   colored_seeds : un tableau de couleur des régions
   adjacences_matrix : un tableau d'adjacence des régions
   return : une liste de clauses *)
let adjacent colored_seeds adjacences_matrix =
  let seeds_number = Array.length colored_seeds and
  adjacent = ref [] in
  for i = 0 to (seeds_number - 1) do
    for i' = 0 to (seeds_number - 1) do
      if adjacences_matrix.(i).(i') then
        for c = 1 to 4 do
          adjacent := [(false, (i, colors.(c))); (false, (i', colors.(c)))]::(!adjacent)
        done;
    done;
  done;
  !adjacent
;;

(* Produit la FNC (liste de contraintes)
   colored_seeds : un tableau de couleur des régions
   adjacences_matrix : un tableau d'adjacence des régions
   return : une liste de clauses *)
let produce_constraints colored_seeds adjacences_matrix =
  (exists colored_seeds)@(unique colored_seeds)@(adjacent colored_seeds adjacences_matrix)
;;

(* Associe une couleur à une germe
   voronoi : un diagramme de Voronoi
   l : la liste à parcourir *)
let rec color_to_seed voronoi l =
  match l with
  | [] -> failwith "Pas d'élément"
  | [(b, (index, color))] -> voronoi.seeds.(index).c <- Some(color)
  | (b, (index, color))::l2 ->
    begin
      voronoi.seeds.(index).c <- Some(color);
      color_to_seed voronoi l2
    end
;;

let rec print_list_color (Some(l)) =
  match l with
  | [] -> print_string "\n"
  | (b, (i, c))::l2 ->
    begin
      print_string ("index de région "^(string_of_int i)^", couleur "^(string_of_color_option (Some(c)))^"\n");
      print_list_color (Some(l2))
    end
;;

(* Renvoie une solution d'un diagramme
   voronoi : un diagramme de Voronoi
   constraints : la liste des contraintes
   return : un diagramme de Voronoi *)
let solve_voronoi voronoi constraints =
  let solution = Solver.solve constraints in
  print_list_color solution;
  match solution with
  | None -> failwith "Aucune solution"
  | Some(l) -> color_to_seed voronoi l
    ;
  voronoi
;;

(* Colorie une région non encore coloriée
   voronoi : un diagramme de Voronoi
   index : l'index dans le tableau de germes de la région
   color : une couleur
   matrix_regions : la matrice des régions associée à voronoi *)
let color_region voronoi region color matrix_regions =
  region.c <- Some(color);
  draw_voronoi voronoi matrix_regions 1
;;

(* Modifie une région coloriée : si color vaut white, la couleur est supprimée, sinon la couleur est modifiée
   voronoi : un diagramme de Voronoi
   initial_colored_seeds : le tableau initial des couleurs des régions
   region : la région du diagramme
   color : une couleur *)
let change_region voronoi initial_colored_seeds index_region color matrix_regions =
  if color = white then
    voronoi.seeds.(index_region).c <- None
  else
    voronoi.seeds.(index_region).c <- Some(color);
  draw_voronoi voronoi matrix_regions 1
;;

(* Vérifie si la configuration est gagnante
   voronoi : un diagramme de Voronoi
   adjacences_matrix : un tableau d'adjacence des régions
   return : false s'il y a au moins une erreur, true sinon *)
let win voronoi adjacences_matrix =
  let win = ref true in
  let seeds_number = Array.length voronoi.seeds in
  for i = 0 to (seeds_number - 1) do
    match voronoi.seeds.(i).c with
    | None -> win := false
    | Some(rgb) -> begin
        for j = 0 to (seeds_number - 1) do
          if adjacences_matrix.(i).(j) = true then
            if voronoi.seeds.(j).c = Some(rgb) then
              win := false
        done;
      end;
  done;
  !win
;;

(* Choisit une couleur sur la fenêtre
   wne : wait_next_event [Button_down]
   return : une couleur option *)
let pick_color x y =
  let color_option = ref None and
  width = 100 and
  height = (size_y ()) / 6 in
  let side = ((min width height) - 4) in
  let middle = size_x () - 250 in
  let x_corner = middle - side / 2 and
    y_corner = size_y () / 6 in
  if (x <= x_corner + side) && (x >= x_corner) then
    for i = 0 to 4 do
      if (y >= y_corner * i + 2) && (y <= y_corner * i + 2 + side) then
        color_option := Some(colors.(i))
    done;
  !color_option
;;

(* Choisit une région du graphe
   voronoi : un diagramme de Voronoi
   wne : wait_next_event [Button_down]
   matrix_regions : la matrice des régions associée à voronoi
   return : l'indice d'une région du diagramme *)
let pick_index_region voronoi x y matrix_regions =
  matrix_regions.(x).(y)
;;

(* Ecrit un message dans la zone dédiée de la fenêtre
   message : le message à afficher *)
let draw_string_message message =
  synchronize ();
  moveto (size_x () - 280) (11 * size_y () / 12);
  set_color black;
  draw_string message;
  (* let oc = open_out message in
  flush oc; *)
  synchronize ()
;;

(* let delete_string_message message =
  moveto (size_x () - 280) (11 * size_y () / 12);
  set_color white;
  draw_string message;
  synchronize ()
;; *)

(* Supprime un diagramme de la liste de diagrammes
   diagram : le diagramma à supprimer
   diagram_list : la liste de diagrammes
   return : une liste de diagrammes *)
let rec delete_diagram diagram diagram_list =
  match diagram_list with
  | [] -> []
  | d::l ->
    begin
      if d = diagram then
        l
      else
        d::(delete_diagram diagram l)
    end
;;

(* Renvoie un entier selon la suite du jeu
   return : l'indice dans colors de la couleur si une couleur est séléctionnée ; (Array.length colors) si le bouton "Solution" est séléctionné *)
let rec first_click () =
  let return_value = ref (-1) in
  let wne = wait_next_event [Button_down] in
  let x = wne.mouse_x and
    y = wne.mouse_y in
  let color_option = pick_color x y in
  if color_option <> None then    (* si la sélection est une couleur *)
    begin
      for i = 0 to ((Array.length colors) - 1) do
        if color_option = Some(colors.(i)) then
          return_value := i       (* l'indice de la couleur dans colors *)
      done;
      !return_value
    end
  else if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 2) && (y >= size_y () / 3) then    (* si le bouton "Solution est sélectionné" *)
    begin
      return_value := (Array.length colors);
      !return_value
    end
  else
    first_click ()                (* sinon on recommence *)
;;

(* Renvoie un entier selon la suite du jeu
   voronoi : un diagramme de Voronoi
   matrix_regions : la matrice des régions associée à voronoi
   return : l'indice de la région si une région est sélectionnée ; (Array.length voronoi.seeds) si le bouton "Solution" est sélectionné ; l'indice de la couleur * 100 si une couleur est sélectionnée *)
let rec second_click voronoi matrix_regions =
  let return_value = ref (-1) in
  let wne = wait_next_event [Button_down] in
  let x = wne.mouse_x and
    y = wne.mouse_y in
  if x <= Array.length matrix_regions then                                              (* si une région est sélectionnée *)
    begin
      return_value := pick_index_region voronoi x y matrix_regions;   (* l'indice de la région dans le tableau de germes du diagramme *)
      !return_value
    end
  else if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 2) && (y >= size_y () / 3) then    (* si le bouton "Solution est sélectionné" *)
    begin
      return_value := Array.length voronoi.seeds;
      !return_value
    end
  else
    begin
      let c = pick_color x y in
      match c with
      | Some(rgb) ->                                                (* si une couleur est sélectionnée *)
        begin
          for i = 1 to ((Array.length colors) - 1) do
            if rgb = colors.(i) then
              return_value := i * 100
          done;
          !return_value
        end
      | None -> second_click voronoi matrix_regions
    end
;;

(* Fonction principale
   diagram_list : la liste des diagrammes non encore joués *)
let rec main diagram_list =
  let distance = euclidean_distance in
  let game_state = ref 0 in   (* état du jeu *)
  Random.self_init ();
  let index_diagram = Random.int (List.length diagram_list) in
  let diagram = ref (List.nth diagram_list index_diagram) in                (* choix aléatoire d'un diagramme parmi ceux non encore joués *)
  let matrix_regions = regions_voronoi !diagram distance in                 (* matrice des régions du diagramme *)
  let adjacences_matrix = adjacences_voronoi !diagram matrix_regions in     (* matrice des adjacences des régions du diagramme *)
  let initial_colored_seeds = is_seed_colored !diagram in                   (* tableau des couleurs pour chaque région au début du jeu *)

  game_state := 1;

  draw_voronoi !diagram matrix_regions !game_state;    (* ouverture du graphe *)

  auto_synchronize false;

  while !game_state = 1 do    (* au cours du jeu *)
    let color_option = ref None and
    index_region = ref (-1) in

    let s = ref true and
      g = ref true in

    let integer1 = first_click () in      (* première action *)
    if integer1 = Array.length colors then                                  (* si le bouton "Solution" a été sélectionné *)
      begin
        let colored_seeds = is_seed_colored !diagram in                           (* tableau des couleurs pour chaque région à ce stade du jeu *)
        let constraints = produce_constraints colored_seeds adjacences_matrix in
        diagram := solve_voronoi !diagram constraints;                            (* diagramme résolu *)
        let solved_matrix_regions = regions_voronoi !diagram distance in          (* matrice des régions du diagramme résolu *)
        draw_voronoi !diagram solved_matrix_regions !game_state;                  (* tracé du graphe résolu *)
        game_state := 2;
        s := false
      end
    else if (integer1 >= 0) && (integer1 < Array.length colors) then        (* si une couleur a été sélectionnée *)
      color_option := Some(colors.(integer1));

    while !s do
      begin
        let integer2 = second_click !diagram matrix_regions in                        (* deuxième action *)
        if integer2 = Array.length !diagram.seeds then                                (* si le bouton "Solution" a été sélectionné *)
          begin
            let colored_seeds = is_seed_colored !diagram in                                 (* tableau des couleurs pour chaque région à ce stade du jeu *)
            let constraints = produce_constraints colored_seeds adjacences_matrix in
            diagram := solve_voronoi !diagram constraints;                                  (* diagramme résolu *)
            let solved_matrix_regions = regions_voronoi !diagram distance in                (* matrice des régions du diagramme résolu *)
            draw_voronoi !diagram solved_matrix_regions !game_state;                        (* tracé du graphe résolu *)
            game_state := 2;
            s := false
          end
        else if integer2 < Array.length !diagram.seeds then                           (* si une région a été sélectionnée *)
          begin
            index_region := integer2;
            s := false
          end
        else if integer2 >= 100 then                                                  (* si une nouvelle couleur a été sélectionnée *)
          begin
            let index = integer2 / 100 in
            color_option := Some(colors.(index))
          end
      end
    done;

    if !game_state = 2 then
      g := false;

    if !g then
      begin
        let color = ref grey in
        begin
          match !color_option with
          | Some(rgb) -> (color := rgb)
          | None -> (color := white)
        end;
        let region = !diagram.seeds.(!index_region) in    (* choix d'une région à colorer *)
        match region.c with
        | None ->                                         (* si la région est vide *)
          begin
            color_region !diagram region !color matrix_regions;
            if (win !diagram adjacences_matrix) then      (* si la combinaison est gagnante *)
              begin
                draw_string_message "Felicitations ! Vous avez gagne !";
                game_state := 2
              end
          end
        | Some(rgb) ->                                            (* si la région est colorée *)
          begin
            if initial_colored_seeds.(!index_region) = None then   (* si la région n'est initialement pas colorée *)
              begin
                change_region !diagram initial_colored_seeds !index_region !color matrix_regions;
                synchronize ()
              end;
            if (win !diagram adjacences_matrix) then              (* si la combinaison est gagnante *)
              begin
                draw_string_message "Felicitations ! Vous avez gagne !";
                game_state := 2
              end
          end
      end
  done;

  if !game_state = 2 then   (* si le joueur a gagné ou s'il a affiché la solution *)
    begin
      draw_voronoi !diagram matrix_regions !game_state;
      let wne = wait_next_event [Button_down] in
      let x = wne.mouse_x and
        y = wne.mouse_y in
      if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 3) && (y >= size_y () / 6) then   (* si on a cliqué sur le bouton "Rejouer" *)
        begin
          let new_diagram_list = delete_diagram !diagram diagram_list in
          if List.length new_diagram_list = 0 then    (* s'il n'y a plus aucune grille à jouer *)
            draw_string_message "Plus aucune grille a jouer !"
          else                                        (* s'il reste une ou des grilles non encore jouée(s) *)
            begin
              close_graph ();
              main new_diagram_list                   (* crée une nouvelle partie *)
            end
        end
      else if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= 2 * size_y () / 3) && (y >= size_y () / 2) then    (* si on a cliqué sur le bonton "Quitter" *)
        close_graph ()
    end
;;

main diagrams;;
