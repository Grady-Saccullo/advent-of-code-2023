type point = {
  row : int;
  mutable num : string;
  mutable start_col : int;
  mutable end_col : int;
}

let is_symbol ch =
  match ch with '.' -> None | '0' .. '9' -> None | v -> Some v

let is_num ch = match ch with '0' .. '9' -> Some ch | _ -> None

(* [ *)
(*     [1, 1, 1, 0, 0] *)
(*     [1, 1, 1, 1, 0] *)
(*     [0, 1, 1  1, 0] *)
(*     [0, 1, 1, 1, 0] *)
(*     [0, 0, 0, 0, 0] *)
(* ] *)

let fill_valid_location locs_arr cur_row cur_col =
  let is_top = cur_row == 0 in
  let is_bottom = cur_row + 1 == Array.length locs_arr in
  let is_left = cur_col == 0 in
  let is_right = cur_col + 1 == Array.length locs_arr.(0) in
  let fill row start_col end_col =
    for i = start_col to end_col do
      locs_arr.(row).(i) <- 1
    done
  in

  let start_col = ref cur_col in
  let end_col = ref cur_col in
  let top_row = ref cur_row in
  let bottom_row = ref cur_row in
  if not is_left then start_col := cur_col - 1;
  if not is_right then end_col := cur_col + 1;
  if not is_top then top_row := cur_row - 1;
  if not is_bottom then bottom_row := cur_row + 1;
  fill !top_row !start_col !end_col;
  fill !bottom_row !start_col !end_col;
  fill cur_row !start_col !end_col

let build_locations lines row_len col_len =
  let valid_locations = Array.make_matrix col_len row_len Int.zero in
  let num_locations = ref [] in
  let non_symbol_match i j ch cur_location is_prev_num =
    match is_num ch with
    | None ->
        if !is_prev_num then (
          num_locations := !cur_location :: !num_locations;
          cur_location := { row = i; num = ""; start_col = 0; end_col = 0 };
          is_prev_num := false)
    | Some num ->
        !cur_location.num <- !cur_location.num ^ Char.escaped num;
        Printf.printf "num: %s\n" !cur_location.num;
        if not !is_prev_num then (
          !cur_location.start_col <- j;
          !cur_location.end_col <- j;
          is_prev_num := true)
        else !cur_location.end_col <- j
  in
  for i = 0 to row_len - 1 do
    let cur_location = ref { row = i; num = ""; start_col = 0; end_col = 0 } in
    let is_prev_num = ref false in
    for j = 0 to col_len - 1 do
      let ch = (List.nth lines i).[j] in
      match is_symbol ch with
      | Some _ ->
          fill_valid_location valid_locations i j;
          if !is_prev_num then (
            num_locations := !cur_location :: !num_locations;
            cur_location := { row = i; num = ""; start_col = 0; end_col = 0 };
            is_prev_num := false)
      | None -> non_symbol_match i j ch cur_location is_prev_num
    done;
    if !cur_location.num <> "" then
      num_locations := !cur_location :: !num_locations
  done;
  (valid_locations, !num_locations)

let runner file =
  let lines = Shared.File.read_lines file in
  let row_len = List.length lines in
  let line_col_len = List.nth lines 0 |> String.length in

  let valid_locations, num_locations =
    build_locations lines row_len line_col_len
  in

  let sum = ref 0 in
  List.rev num_locations
  |> List.iter (fun loc ->
         let num = int_of_string loc.num in
         let start_col = loc.start_col in
         let end_col = loc.end_col in
         let has_set = ref false in
         for i = start_col to end_col do
           if valid_locations.(loc.row).(i) == 1 then
             if not !has_set then (
               sum := !sum + num;
               has_set := true)
         done);
  !sum
