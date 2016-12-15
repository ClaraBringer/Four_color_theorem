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

(* Dessine les cercles de couleur *)
let draw_color_circles () =
  let colors = [|white; red; green; blue; yellow|] in
  for i = 0 to 4 do
    let x = size_x () - 250 and
    y = (size_y () * (i + 1)) / 6 - (size_y ()) / 12 and
    circle_width = 100 and
      circle_height = (size_y ()) / 6 in
    let radius = ((min circle_width circle_height) - 5) / 2 in
    set_color black;
    draw_circle x y radius;
    set_color colors.(i);
    fill_circle x y (radius - 1);
  done;
;;

(* Dessine les boutons selon le moment du jeu *)
let draw_buttons game_state =
  set_color black;
  if game_state = 1 then
    let x = size_x () - 190 and
    y = size_y () / 3 and
    width = 180 and
    height = (size_y () / 2) - (size_y () / 3) in
    draw_rect x y width height;
    set_color grey;
    fill_rect (x + 1) (y + 1) (width - 2) (height - 2);
    moveto (x + width/3) (y + height/2);
    set_color black;
    draw_string "Solution"
;;

(* Dessine le diagramme
   voronoi : un diagramme de Voronoi
   matrix_regions : la matrice des régions associée à voronoi *)
let draw_voronoi voronoi matrix_regions =
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
  draw_color_circles ();
  draw_buttons 1;
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
  let colors = [|red; green; blue; yellow|] in
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
  let colors = [|red; green; blue; yellow|] in
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
  let colors = [|red; green; blue; yellow|] in
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
   color : une couleur *)
let color_region voronoi index color =
  if voronoi.seeds.(index).c = None then
    voronoi.seeds.(index).c <- Some(color)
;;

(* Modifie une région coloriée : si color vaut white, la couleur est supprimée, sinon la couleur est modifiée
   voronoi : un diagramme de Voronoi
   initial_colored_seeds : le tableau initial des couleurs des régions
   index : l'index dans le tableau de germes de la région
   color : une couleur *)
let change_region voronoi initial_colored_seeds index color =
  match initial_colored_seeds.(index).c with
  | None -> begin
      if color = white then
        voronoi.seeds.(index).c <- None
      else
        voronoi.seeds.(index).c <- Some(color)
    end
  | Some(rgb) -> print_string "Vous ne pouvez pas modifier cette région !"
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

(* Fonction main *)
let main =
  Random.self_init ();
  let index_diagram = Random.int (Array.length diagrams) in
  let diagram = diagrams.(index_diagram) in
  let matrix_regions = regions_voronoi diagram euclidean_distance in

  draw_voronoi diagram matrix_regions;
  wait_next_event [Button_down];

  (* let initial_colored_seeds = is_seed_colored diagram and
  adjacences_matrix = adjacences_voronoi diagram matrix_regions in
  let constraints = produce_constraints initial_colored_seeds adjacences_matrix in
  let solved_diagram = solve_voronoi diagram constraints in *)

;;

main;;
