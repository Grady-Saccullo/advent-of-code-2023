let () =
  let cwd = Sys.getcwd () in
  let file = String.cat cwd "/files/day_5/dataset.txt" in
  let part_1 = Day_5.Part_1.runner file in
  Printf.printf "Part one result: %i\n" part_1;
  (* let part_2 = Day_4.Part_2.runner file in *)
  (* Printf.printf "Part two Result: %i\n" part_2 *)
