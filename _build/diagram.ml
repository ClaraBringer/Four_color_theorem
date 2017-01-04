open Graphics;;
open Voronoi;;
open Examples;;

let grey = rgb 200 200 200;;

type game_state = Initialization | In_game | Between_games;;

exception No_solution_exception;;

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

(* Crée une distance
 * q : un float
 * return : la distance associée *)
let make_distance q =
  fun (x1, y1) (x2, y2) -> (float_of_int (abs (x1 - x2)))**q +. (float_of_int (abs (y1 - y2)))**q
;;

(* La distance taxicab *)
let taxicab_distance = make_distance 1.;;

(* La distance euclidienne *)
let euclidean_distance = make_distance 2.;;

(* Construit la matrice des régions du diagramme
 * voronoi : un diagramme de Voronoi
 * distance_function : une fonction de distance
 * return : une matrice d'entiers *)
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

(* Construit la matrice d'adjacence des régions du diagramme
 * voronoi : un diagramme de Voronoi
 * matrix_regions : la matrice des régions associée à voronoi
 * return : une matrice de booléens *)
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

(* Ecrit un message dans la zone dédiée de la fenêtre
 * message : le message à afficher *)
let draw_string_message message =
  synchronize ();
  moveto (size_x () - 280) (11 * size_y () / 12);
  set_color black;
  draw_string message;
  synchronize ()
;;

(* Dessine les carrés de couleur
 * colors : le tableau des couleurs du diagramme *)
let draw_color_squares colors =
  for i = 0 to (Array.length colors - 1) do
    let width = 100 and
    height = (size_y ()) / (Array.length colors + 1) in
    let side = ((min width height) - 4) in
    let middle = size_x () - 250 in
    let x = middle - side / 2 and
      y = (size_y () * i) / (Array.length colors + 1) + 2 in
    set_color black;
    draw_rect x y side side;
    set_color colors.(i);
    fill_rect (x + 1) (y + 1) (side - 2) (side - 2);
  done;
;;

(* Dessine un bouton
 * x : abscisse du coin inférieur gauche du rectangle
 * y : ordonnée du coin inférieur gauche du rectangle
 * width : largeur du rectangle
 * height : hauteur du rectangle
 * message : message du bouton *)
let draw_button x y width height message =
  draw_rect x y width height;
  set_color grey;
  fill_rect (x + 1) (y + 1) (width - 2) (height - 2);
  moveto (x + width/3) (y + height/2);
  set_color black;
  draw_string message
;;

(* Dessine les boutons selon le moment du jeu
 * game_state : le moment du jeu *)
let draw_buttons game_state =
  set_color black;
  let x = size_x () - 190 and
    width = 180 and
    height = size_y () / 6 in
  if game_state = In_game then
    let y = size_y () / 3 in
    draw_button x y width height "Solution"
  else if game_state = Between_games then
    let y_play = size_y () / 6 and
    y_quit = size_y () / 2 in
    draw_button x y_play width height "Play again";
    draw_button x y_quit width height "Quit"
;;

(* Dessine le diagramme
 * voronoi : un diagramme de Voronoi
 * matrix_regions : la matrice des régions associée à voronoi
 * game_state : le moment du jeu
 * colors : le tableau des couleurs du diagramme *)
let draw_voronoi voronoi matrix_regions game_state colors =
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
  draw_color_squares colors;
  draw_buttons game_state;
  synchronize ()
;;

(* Construit le tableau des couleurs des régions
 * voronoi : un diagramme de Voronoi
 * return : un tableau de color option *)
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

(* Vérifie que chaque région a au moins une couleur
 * colored_seeds : un tableau de couleur des région
 * colors : le tableau des couleurs du diagramme
 * return : une liste de clauses *)
let exists colored_seeds colors  =
  let seeds_number = Array.length colored_seeds and
  exist_array = ref [] in
  for i = 0 to (seeds_number - 1) do
    let clause = ref [] in
    match colored_seeds.(i) with
    | Some(rgb) -> clause := (true, (i, rgb))::(!clause)
    | None ->
      begin
        for c = 1 to (Array.length colors - 1) do
          clause := (true, (i, colors.(c)))::(!clause)
        done;
        exist_array := (!clause)::(!exist_array)
      end
  done;
  !exist_array
;;

(* Vérifie que chaque région a au plus une couleur
 * colored_seeds : un tableau de couleur des régions
 * colors : le tableau des couleurs du diagramme
 * return : une liste de clauses *)
let unique colored_seeds colors =
  let seeds_number = Array.length colored_seeds and
  unique_array = ref [] in
  for i = 0 to (seeds_number - 1) do
    match colored_seeds.(i) with
    | None ->
      begin
        for c = 1 to (Array.length colors - 1) do
          for c' = 1 to (Array.length colors - 1) do
            if c <> c' then
              unique_array := [(false, (i, colors.(c))); (false, (i, colors.(c')))]::(!unique_array)
          done;
        done;
      end
    | Some(rgb) ->
      begin
        for c = 1 to (Array.length colors - 1) do
          if rgb <> colors.(c) then
            unique_array := [(false, (i, rgb)); (false, (i, colors.(c)))]::(!unique_array)
        done;
      end
  done;
  !unique_array
;;

(* Vérifie que deux régions adjacentes n'ont pas la même couleur
 * colored_seeds : un tableau de couleur des régions
 * adjacences_matrix : un tableau d'adjacence des régions
 * colors : le tableau des couleurs du diagramme
 * return : une liste de clauses *)
let adjacent colored_seeds adjacences_matrix colors =
  let seeds_number = Array.length colored_seeds and
  adjacent = ref [] in
  for i = 0 to (seeds_number - 1) do
    match colored_seeds.(i) with
    | None ->
      begin
        for i' = 0 to (seeds_number - 1) do
          if adjacences_matrix.(i).(i') then
            match colored_seeds.(i') with
            | None ->
              begin
                for c = 1 to (Array.length colors - 1) do
                  adjacent := [(false, (i, colors.(c))); (false, (i', colors.(c)))]::(!adjacent)
                done;
              end
            | Some(rgb) -> adjacent := [(false, (i, rgb))]::(!adjacent)
        done;
      end
    | Some(rgb1) ->
      begin
        for i' = 0 to (seeds_number - 1) do
          if adjacences_matrix.(i).(i') then
            match colored_seeds.(i') with
            | None -> adjacent := [(false, (i', rgb1))]::(!adjacent)
            | Some(rgb2) ->
              begin
                if rgb1 = rgb2 then
                  begin
                    print_string "No solution!\n";
                    raise No_solution_exception
                  end
              end
        done;
      end
  done;
  !adjacent
;;

(* Produit la FNC (liste de contraintes)
 * colored_seeds : un tableau de couleur des régions
 * colors : le tableau des couleurs du diagramme
 * adjacences_matrix : un tableau d'adjacence des régions
 * return : une liste de clauses *)
let produce_constraints colored_seeds colors adjacences_matrix =
  (exists colored_seeds colors)@(unique colored_seeds colors)@(adjacent colored_seeds adjacences_matrix colors)
;;

(* Associe une couleur d'une liste à une germe
 * voronoi : un diagramme de Voronoi
 * l : la liste à parcourir *)
let rec color_to_seed voronoi l =
  match l with
  | [] -> failwith "Pas d'élément\n"
  | [(b, (index, color))] -> voronoi.seeds.(index).c <- Some(color)
  | (b, (index, color))::l2 ->
    begin
      voronoi.seeds.(index).c <- Some(color);
      color_to_seed voronoi l2
    end
;;

(* Renvoie une solution d'un diagramme
 * voronoi : un diagramme de Voronoi
 * constraints : la liste des contraintes
 * return : un diagramme de Voronoi *)
let solve_voronoi voronoi constraints =
  let solution = Solver.solve constraints in
  match solution with
  | None ->
    begin
      print_string "No solution!\n";
      raise No_solution_exception
    end
  | Some(l) -> color_to_seed voronoi l
    ;
  voronoi
;;

(* Colorie une région non encore coloriée
 * voronoi : un diagramme de Voronoi
 * region : une région
 * color : une couleur
 * matrix_regions : la matrice des régions associée à voronoi
 * colors : le tableau des couleurs du diagramme *)
let color_region voronoi region color matrix_regions colors =
  region.c <- Some(color);
  draw_voronoi voronoi matrix_regions In_game colors
;;

(* Modifie une région coloriée : si color vaut white, la couleur est supprimée, sinon la couleur est modifiée
 * voronoi : un diagramme de Voronoi
 * region : la région du diagramme
 * color : une couleur
 * matrix_regions : la matrice des régions associée à voronoi
 * colors : le tableau des couleurs du diagramme *)
let change_region voronoi region color matrix_regions colors =
  if color = white then
    region.c <- None
  else
    region.c <- Some(color);
  draw_voronoi voronoi matrix_regions In_game colors
;;

(* Vérifie si la configuration est gagnante
 * voronoi : un diagramme de Voronoi
 * adjacences_matrix : un tableau d'adjacence des régions
 * return : false s'il y a au moins une erreur, true sinon *)
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
 * x : abscisse du point sélectionné
 * y : ordonnée du point sélectionné
 * colors : le tableau des couleurs du diagramme
 * return : un color option *)
let pick_color x y colors =
  let color_option = ref None and
  width = 100 and
  height = (size_y ()) / (Array.length colors + 1) in
  let side = ((min width height) - 4) in
  let middle = size_x () - 250 in
  let x_corner = middle - side / 2 and
    y_corner = size_y () / (Array.length colors + 1) in
  if (x <= x_corner + side) && (x >= x_corner) then
    for i = 0 to (Array.length colors - 1) do
      if (y >= y_corner * i + 2) && (y <= y_corner * i + 2 + side) then
        color_option := Some(colors.(i))
    done;
  !color_option
;;

(* Choisit une région du graphe
 * voronoi : un diagramme de Voronoi
 * x : abscisse du point sélectionné
 * y : ordonnée du point sélectionné
 * matrix_regions : la matrice des régions associée à voronoi
 * return : l'indice d'une région du diagramme *)
let pick_index_region voronoi x y matrix_regions =
  matrix_regions.(x).(y)
;;

(* Renvoie un entier selon la suite du jeu
 * colors : le tableau des couleurs du diagramme
 * return : l'indice dans colors de la couleur si une couleur est séléctionnée ; (Array.length colors) si le bouton "Solution" est séléctionné *)
let rec first_click colors =
  let return_value = ref (-1) in
  let wne = wait_next_event [Button_down] in
  let x = wne.mouse_x and
    y = wne.mouse_y in
  let color_option = pick_color x y colors in
  if color_option <> None then                (* si la sélection est une couleur *)
    begin
      for i = 0 to ((Array.length colors) - 1) do
        if color_option = Some(colors.(i)) then
          return_value := i                   (* l'indice de la couleur dans colors *)
      done;
      !return_value
    end
  else if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 2) && (y >= size_y () / 3) then    (* si le bouton "Solution" est sélectionné *)
    begin
      return_value := (Array.length colors);
      !return_value
    end
  else
    first_click colors                        (* sinon on recommence *)
;;

(* Renvoie un entier selon la suite du jeu
 * voronoi : un diagramme de Voronoi
 * matrix_regions : la matrice des régions associée à voronoi
 * colors : le tableau des couleurs du diagramme
 * return : l'indice de la région si une région est sélectionnée ; (Array.length voronoi.seeds) si le bouton "Solution" est sélectionné ; l'indice de la couleur * 100 si une couleur est sélectionnée *)
let rec second_click voronoi matrix_regions colors =
  let return_value = ref (-1) in
  let wne = wait_next_event [Button_down] in
  let x = wne.mouse_x and
    y = wne.mouse_y in
  if x <= Array.length matrix_regions then                  (* si une région est sélectionnée *)
    begin
      return_value := pick_index_region voronoi x y matrix_regions;   (* l'indice de la région dans le tableau de germes du diagramme *)
      !return_value
    end
  else if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 2) && (y >= size_y () / 3) then    (* si le bouton "Solution" est sélectionné *)
    begin
      return_value := Array.length voronoi.seeds;
      !return_value
    end
  else
    begin
      let c = pick_color x y colors in
      match c with
      | Some(rgb) ->                                        (* si une couleur est sélectionnée *)
        begin
          for i = 1 to ((Array.length colors) - 1) do
            if rgb = colors.(i) then
              return_value := i * 100
          done;
          !return_value
        end
      | None -> second_click voronoi matrix_regions colors  (* sinon on recommence *)
    end
;;

(* Renvoie un entier selon la suite du jeu
 * return : 0 si le bouton "Play again" est sélectionné, 1 si le bouton "Quit" est sélectionné *)
let rec third_click () =
  let return_value = ref (-1) in
  let wne = wait_next_event [Button_down] in
  let x = wne.mouse_x and
    y = wne.mouse_y in
  if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 3) && (y >= size_y () / 6) then             (* si on a cliqué sur le bouton "Play again" *)
    begin
      return_value := 0;
      !return_value
    end
  else if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= 2 * size_y () / 3) && (y >= size_y () / 2) then    (* si on a cliqué sur le bonton "Quit" *)
    begin
      return_value := 1;
      !return_value
    end
  else
    third_click ()                                                                                                    (* sinon on recommence *)
;;

(* Supprime un diagramme de la liste de diagrammes
 * diagram : le diagramme à supprimer
 * diagram_list : la liste de diagrammes
 * return : une liste de diagrammes *)
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

let colors_array voronoi =
  let colors_list = ref [] in
  let colors_number = ref 0 in                              (* le nombre de couleurs dur le diagramme *)
  for i = 0 to (Array.length voronoi.seeds - 1) do
    match voronoi.seeds.(i).c with
    | None -> colors_list := !colors_list
    | Some(rgb) ->
      begin
        if not (List.mem rgb !colors_list) then
          begin
            colors_list := rgb::(!colors_list);
            colors_number := !colors_number + 1
          end
      end
  done;

  let colors = Array.make (!colors_number + 1) white in     (* le tableau des couleurs du diagramme *)
  for i = 0 to (Array.length voronoi.seeds - 1) do
    match voronoi.seeds.(i).c with
    | Some(rgb) ->
      begin
        let already_exists = ref false in
        for j = 1 to (!colors_number) do
          if colors.(j) = rgb then
            already_exists := true
        done;
        if not !already_exists then
          for j = 1 to (!colors_number) do
            if colors.(j) = white && not !already_exists then
              begin
                colors.(j) <- rgb;
                already_exists := true
              end
          done;
      end
    | None -> colors.(0) <- white
      ;
  done;
  colors
;;

(* Fonction principale
 * diagram_list : la liste des diagrammes non encore joués *)
let rec main diagram_list =
  let distance = euclidean_distance in
  let game_state = ref Initialization in           (* initialisation du jeu *)
  Random.self_init ();
  let index_diagram = Random.int (List.length diagram_list) in
  let diagram = ref (List.nth diagram_list index_diagram) in                (* choix aléatoire d'un diagramme parmi ceux non encore joués *)
  let matrix_regions = regions_voronoi !diagram distance in                 (* matrice des régions du diagramme *)
  let adjacences_matrix = adjacences_voronoi !diagram matrix_regions in     (* matrice des adjacences des régions du diagramme *)
  let initial_colored_seeds = is_seed_colored !diagram in                   (* tableau des couleurs pour chaque région au début du jeu *)

  let colors = colors_array !diagram in

  game_state := In_game;                    (* début du jeu *)

  draw_voronoi !diagram matrix_regions !game_state colors;    (* ouverture du graphe *)

  auto_synchronize false;

  let has_won = ref false in

  while !game_state = In_game do    (* au cours du jeu *)
    let color_option = ref None and
    index_region = ref (-1) in

    let s = ref true and      (* si le second click doit être exécuté *)
      g = ref true in         (* si une région doit être colorée *)

    let action1 = first_click colors in       (* première action *)
    if action1 = Array.length colors then                                 (* si le bouton "Solution" a été sélectionné *)
      begin
        let colored_seeds = is_seed_colored !diagram in                           (* tableau des couleurs pour chaque région à ce stade du jeu *)
        let constraints = produce_constraints colored_seeds colors adjacences_matrix in
        diagram := solve_voronoi !diagram constraints;                            (* diagramme résolu *)
        let solved_matrix_regions = regions_voronoi !diagram distance in          (* matrice des régions du diagramme résolu *)
        draw_voronoi !diagram solved_matrix_regions !game_state colors;           (* tracé du graphe résolu *)
        game_state := Between_games;                    (* fin du jeu *)
        s := false
      end
    else if (action1 >= 0) && (action1 < Array.length colors) then        (* si une couleur a été sélectionnée *)
      color_option := Some(colors.(action1));

    while !s do
      begin
        let action2 = second_click !diagram matrix_regions colors in        (* deuxième action *)
        if action2 = Array.length !diagram.seeds then                     (* si le bouton "Solution" a été sélectionné *)
          begin
            let colored_seeds = is_seed_colored !diagram in                       (* tableau des couleurs pour chaque région à ce stade du jeu *)
            let constraints = produce_constraints colored_seeds colors adjacences_matrix in
            diagram := solve_voronoi !diagram constraints;                        (* diagramme résolu *)
            let solved_matrix_regions = regions_voronoi !diagram distance in      (* matrice des régions du diagramme résolu *)
            draw_voronoi !diagram solved_matrix_regions !game_state colors;       (* tracé du graphe résolu *)
            game_state := Between_games;                    (* fin du jeu *)
            s := false
          end
        else if action2 < Array.length !diagram.seeds then                (* si une région a été sélectionnée *)
          begin
            index_region := action2;
            s := false
          end
        else if action2 >= 100 then                                       (* si une nouvelle couleur a été sélectionnée *)
          begin
            let index = action2 / 100 in
            color_option := Some(colors.(index))
          end
      end
    done;

    if !game_state = Between_games then
      g := false;

    if !g then
      begin
        let color = ref grey in
        begin
          match !color_option with
          | Some(rgb) -> (color := rgb)
          | None -> (color := white)
        end;
        let region = !diagram.seeds.(!index_region) in    (* la région à colorer *)
        match region.c with
        | None ->                                         (* si la région est vide *)
          begin
            color_region !diagram region !color matrix_regions colors;
            if (win !diagram adjacences_matrix) then                (* si la combinaison est gagnante *)
              begin
                has_won := true;
                game_state := Between_games
              end
          end
        | Some(rgb) ->                                    (* si la région est colorée *)
          begin
            if initial_colored_seeds.(!index_region) = None then    (* si la région n'est initialement pas colorée *)
              begin
                change_region !diagram region !color matrix_regions colors;
                synchronize ()
              end;
            if (win !diagram adjacences_matrix) then                (* si la combinaison est gagnante *)
              begin
                has_won := true;
                game_state := Between_games
              end
          end
      end
  done;

  if !game_state = Between_games then     (* si le joueur a gagné ou s'il a affiché la solution *)
    begin
      draw_voronoi !diagram matrix_regions !game_state colors;
      if !has_won then
        draw_string_message "Congratulations! You won!";
      let action = third_click () in
      if action = 0 then         (* si on a cliqué sur le bouton "Play again" *)
        begin
          let new_diagram_list = delete_diagram !diagram diagram_list in
          if List.length new_diagram_list = 0 then    (* s'il n'y a plus aucune grille à jouer *)
            begin
              close_graph ();
              print_string "No more diagram!\n"
            end
          else                                        (* s'il reste une ou des grilles non encore jouée(s) *)
            begin
              close_graph ();
              main new_diagram_list                   (* crée une nouvelle partie *)
            end
        end
      else if action = 1 then    (* si on a cliqué sur le bonton "Quit" *)
        close_graph ()
    end
;;

main list;;
