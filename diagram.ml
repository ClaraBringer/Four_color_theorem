open Graphics;;
open Voronoi;;
open Examples;;

let grey = rgb 200 200 200

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

(* La distance euclidienne*)
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
    height = (size_y () / 2) - (size_y () / 3) in
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
    for c = 0 to 3 do
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
    for c = 0 to 3 do
      for c' = 0 to 3 do
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
        for c = 0 to 3 do
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

(* Renvoie une solution d'un diagramme
   voronoi : un diagramme de Voronoi
   constraints : la liste des contraintes
   return : un diagramme de Voronoi *)
let solve_voronoi voronoi constraints =
  let solution = Solver.solve constraints in
  match solution with
  | None -> failwith "Aucune solution"
  | Some(l) -> begin
      for i = 0 to ((List.length l) - 1) do
        let index = fst (snd (List.nth l i)) and
          color = snd (snd (List.nth l i)) in
        voronoi.seeds.(index).c <- Some(color)
      done;
    end;
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
let change_region voronoi initial_colored_seeds index_region color =
  let region = voronoi.seeds.(index_region) in
  match region.c with
  | None ->
    begin
      if color <> white then
        region.c <- Some(color)
    end
  | Some(rgb) ->
    begin
      if initial_colored_seeds.(index_region) = None then
        region.c <- Some(color)
    end
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

(* Choisis une couleur sur la fenêtre
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
  color_option
;;

(* Choisis une région du graphe
   voronoi : voronoi : un diagramme de Voronoi
   wne : wait_next_event [Button_down]
   matrix_regions : la matrice des régions associée à voronoi
   return : l'indice d'une région du diagramme *)
let pick_region voronoi wne matrix_regions =
  let x = wne.mouse_x and
  y = wne.mouse_y in
  let index = matrix_regions.(x).(y) in
  index
;;

let draw_string_message message =
  set_color grey;
  fill_rect (size_x () - 200) (10 * size_y () / 12) 200 (2 * size_y () / 12);
  moveto (size_x () - 180) (11 * size_y () / 12);
  set_color black;
  draw_string message
;;

(* Fonction principale *)
let main =
  let distance = euclidean_distance in
  let game_state = ref 0 in
  Random.self_init ();
  let index_diagram = Random.int (Array.length diagrams) in
  let diagram = diagrams.(index_diagram) in
  let matrix_regions = regions_voronoi diagram distance in
  let adjacences_matrix = adjacences_voronoi diagram matrix_regions in
  let initial_colored_seeds = is_seed_colored diagram in

  game_state := 1;

  draw_voronoi diagram matrix_regions !game_state;

  draw_string_message "abcdefghijklmnopqrstuvwxyz1234567890";

  while !game_state = 1 do
    let wne = wait_next_event [Button_down] in
    let x = wne.mouse_x and
    y = wne.mouse_y in
    let color_option = pick_color x y in
    match !color_option with
    | Some(color) ->
      begin
        draw_string_message "La couleur choisie est une couleur";
        let wne2 = wait_next_event [Button_down] in
        let index_region = pick_region diagram wne2 matrix_regions in
        let region = diagram.seeds.(index_region) in
        match region.c with
        | None ->
          begin
            draw_string_message "la région choisie est vide";
            color_region diagram region color matrix_regions;
            draw_string_message "color_region a fonctionné";
            if (win diagram adjacences_matrix) then
              begin
                draw_string_message "Félicitations ! Vous avez gagné !";
                game_state := 2
              end
          end
        | Some(rgb) ->
          begin
            draw_string_message "la région choisie est colorée";
            change_region diagram initial_colored_seeds index_region color;
            draw_string_message "change_region a fonctionné"
          end
      end
    | None ->
      begin
        if (x <= size_x () - 10) && (x >= size_x () - 190) && (y <= size_y () / 2) && (y >= size_y () / 3) then
          begin
            let colored_seeds = is_seed_colored diagram in
            let constraints = produce_constraints colored_seeds adjacences_matrix in
            let solved_diagram = solve_voronoi diagram constraints in
            let solved_matrix_regions = regions_voronoi solved_diagram distance in
            draw_voronoi solved_diagram solved_matrix_regions !game_state
          end;
        game_state := 2
      end
  done;

  (* if !game_state = 2 then
    begin

    end *)
;;

main;;
