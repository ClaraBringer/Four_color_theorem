open Graphics;;
open Voronoi;;
open Examples;;

let grey = rgb 200 200 200;;

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

(* Renvoie une solution d'un diagramme
   voronoi : un diagramme de Voronoi
   constraints : la liste des contraintes
   return : un diagramme de Voronoi *)
let solve_voronoi voronoi constraints =
  let solution = Solver.solve constraints in
  match solution with
  | None -> failwith "Aucune solution"
  | Some(l) -> color_to_seed voronoi l
    ;
    (* begin
      for i = 0 to ((List.length l) - 1) do
        let index = fst (snd (List.nth l i)) and
          color = snd (snd (List.nth l i)) in
        voronoi.seeds.(index).c <- Some(color)
      done;
    end; *)
  voronoi
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

let matrix_regions = regions_voronoi v2 euclidean_distance;;

(* draw_voronoi v2 matrix_regions 1;;

wait_next_event [Button_down];; *)

let constraints = produce_constraints (is_seed_colored v2) (adjacences_voronoi v2 matrix_regions);;

let solution = solve_voronoi v2 constraints;;

draw_voronoi solution matrix_regions 2;;

wait_next_event [Button_down];;
