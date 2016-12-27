let adjacent colored_seeds adjacences_matrix =
  let seeds_number = Array.length colored_seeds and
  adjacent = ref [] in
  for i = 0 to (seeds_number - 1) do
    for i' = 0 to (seeds_number - 1) do
      if adjacences_matrix.(i).(i') then
        match colored_seeds.(i) with
        | None ->
          begin
            match colored_seeds.(i') with
            | None ->
              begin
                for c = 1 to 4 do
                  adjacent := [(false, (i, colors.(c))); (false, (i', colors.(c)))]::(!adjacent)
                done;
              end
            | Some(rgb) -> adjacent := [(false, (i, rgb)); (false, (i', rgb))]::(!adjacent)
          end
        | Some(rgb1) ->
          begin
            match colored_seeds.(i') with
            | None -> adjacent := [(false, (i, rgb1)); (false, (i', rgb1))]::(!adjacent)
            | Some(rgb2) ->
              begin
                adjacent := [(false, (i, rgb1)); (false, (i', rgb1))]::(!adjacent);
                adjacent := [(false, (i, rgb2)); (false, (i', rgb2))]::(!adjacent)
              end
          end
    done;
  done;
  !adjacent
;;
