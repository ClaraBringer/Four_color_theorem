open Graphics;;

type seed = {
  c : color option;
  x : int;
  y : int
}
;;

type voronoi = {
  dim : int * int;
  seeds : seed array
}
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

let v1 = {
  dim = 200,200;
  seeds = [|
    {c = Some red; x=50; y=100};
    {c = Some green; x=100; y=50};
    {c = None; x=100; y=150};
    {c = None; x=150; y=100};
    {c = Some blue; x=100; y=100}
  |]
}

let v2 = {
  dim = 600,600;
  seeds = [|
    {c = None; x=100; y=100};
    {c = Some red; x=125; y=550};
    {c = None; x=250; y=50};
    {c = Some blue; x=150; y=250};
    {c = None; x=250; y=300};
    {c = None; x=300; y=500};
    {c = Some red; x=400; y=100};
    {c = None; x=450; y=450};
    {c = None; x=500; y=250};
    {c = Some yellow; x=575; y=350};
    {c = Some green; x=300; y=300};
    {c = None; x=75; y=470};
  |]
}

let v3 = {
  dim = 600,600;
  seeds = [|
    {c = None; x=100; y=100};
    {c = Some red; x=125; y=550};
    {c = None; x=250; y=50};
    {c = Some blue; x=150; y=250};
    {c = None; x=250; y=300};
    {c = None; x=300; y=500};
    {c = Some red; x=400; y=100};
    {c = None; x=450; y=450};
    {c = None; x=500; y=250};
    {c = None; x=575; y=350};
    {c = Some green; x=300; y=300};
    {c = None; x=75; y=470};
    {c = None; x=10; y=14};
    {c = Some red; x=122; y=55};
    {c = None; x=25; y=345};
    {c = Some blue; x=23; y=550};
    {c = None; x=25; y=30};
    {c = None; x=367; y=530};
    {c = None; x=434; y=10};
    {c = None; x=45; y=50};
    {c = None; x=50; y=25};
    {c = Some yellow; x=578; y=550};
    {c = Some green; x=30; y=350};
    {c = None; x=375; y=47};
  |]
}

let v4 = {
  dim = 800,800;
  seeds = [|
    {c = None; x=100; y=75};
    {c = None; x=125; y=225};
    {c = Some red; x=25; y=255};
    {c = None; x=60; y=305};
    {c = Some blue; x=50; y=400};
    {c = Some green; x=100; y=550};
    {c = Some green; x=150; y=25};
    {c = Some red; x=200; y=55};
    {c = None; x=200; y=200};
    {c = None; x=250; y=300};
    {c = None; x=300; y=450};
    {c = None; x=350; y=10};
    {c = None; x=357; y=75};
    {c = Some yellow; x=450; y=80};
    {c = Some blue; x=400; y=150};
    {c = None; x=550; y=350};
    {c = None; x=400; y=450};
    {c = None; x=400; y=500};
    {c = Some red; x=500; y=75};
    {c = Some green; x=600; y=100};
    {c = Some red; x=700; y=75};
    {c = None; x=578; y=175};
    {c = None; x=750; y=205};
    {c = None; x=520; y=345};
    {c = None; x=678; y=420};
    {c = None; x=600; y=480};
    {c = Some blue; x=650; y=480};
    {c = None; x=750; y=500};
    {c = None; x=600; y=550};
    {c = Some red; x=700; y=550};
  |]
}

let string_of_dim (i, j) =
  (string_of_int i)^("x")^(string_of_int j)
;;

let make_distance q =
  fun (x1, y1) (x2, y2) -> (float_of_int (abs (x1 - x2)))**q +. (float_of_int (abs (y1 - y2)))**q
;;

let euclidean_distance =
  make_distance 2.
;;

let taxicab_distance =
  make_distance 1.
;;

(* Construit la matrice des régions du diagramme *)
let regions_voronoi distance_function voronoi =
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
        if dist < !min_dist then (
          min_dist := dist;
          index := k;
        );
      done;
      matrix.(x).(y) <- !index;
    done;
  done;
  matrix
;;

(* Dessine le diagramme *)
let draw_voronoi distance_function voronoi =
  let matrix = regions_voronoi distance_function voronoi in
  let columns = Array.length matrix and
  lines = Array.length matrix.(0) in
  open_graph (" "^(string_of_int columns)^"x"^(string_of_int lines));
  auto_synchronize false;
  for x = 0 to (columns - 1) do
    for y = 0 to (lines - 1) do
      if (((x > 1) && (matrix.(x).(y) <> matrix.(x-1).(y)))
          || ((x < columns - 1) && (matrix.(x).(y) <> matrix.(x+1).(y)))
          || ((y > 1) && (matrix.(x).(y) <> matrix.(x).(y-1)))
          || ((y < lines - 1) && (matrix.(x).(y) <> matrix.(x).(y+1))))
      then
        set_color black
      else (
        match voronoi.seeds.(matrix.(x).(y)).c with
        | None -> set_color white
        | Some(rgb) -> set_color rgb
      );
      plot x y;
    done;
  done;
  synchronize ()
;;

(* Construit la matrice d'adjacence des régions du diagramme *)
let adjacences_voronoi voronoi matrix =
  let seeds_number = Array.length voronoi.seeds and
  columns = Array.length matrix and
  lines = Array.length matrix.(0) in
  let adjacences = Array.make_matrix seeds_number seeds_number false in
  for h = 0 to (seeds_number - 1)  do
    for k = 0 to (seeds_number - 1) do
      if h <> k then
        begin
          for i = 0 to (columns - 1) do
            for j = 0 to (lines - 1) do
              if (matrix.(i).(j) = h && (((i > 0) && matrix.(i-1).(j) = k)
                                         || ((i < columns - 1) && matrix.(i+1).(j) = k)
                                         || ((j > 0) && matrix.(i).(j-1) = k)
                                         || ((j < lines - 1) && matrix.(i).(j+1) = k)))
              then
                adjacences.(h).(k) <- true
            done;
          done;
        end;
    done;
  done;
  adjacences
;;

(* Construit le tableau des couleurs des régions *)
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

(* Vérifie s'il existe une couleur à une région *)
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

(* Vérifie que chaque région a au plus une couleur *)
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

(* Vérifie que deux régions adjacentes n'ont pas la même couleur *)
let adjacent colored_seeds adjacences =
  let colors = [|red; green; blue; yellow|] in
  let seeds_number = Array.length colored_seeds and
  adjacent_matrix = ref [] in
  for i = 0 to (seeds_number - 1) do
    for i' = 0 to (seeds_number - 1) do
      if adjacences.(i).(i') then
        for c = 0 to 3 do
          adjacent_matrix := [(false, (i, colors.(c))); (false, (i', colors.(c)))]::(!adjacent_matrix)
        done;
    done;
  done;
  !adjacent_matrix
;;

let produce_constraints colored_seeds adjacences_matrix =
  (exists colored_seeds)@(unique colored_seeds)@(adjacent colored_seeds adjacences_matrix)
;;

let colored_seeds = is_seed_colored v1;;
let matrix = regions_voronoi euclidean_distance v1;;
let adjacences_matrix = adjacences_voronoi v1 matrix;;

Solver.solve (produce_constraints colored_seeds adjacences_matrix);;

draw_voronoi (make_distance 2.) v1;;

wait_next_event [Button_down];;
