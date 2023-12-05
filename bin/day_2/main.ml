let () =
  let cwd = Sys.getcwd () in
  let file = String.cat cwd "/files/day_2/dataset.txt" in
  let part_1 = Day_2.Part_1.runner file in
  Printf.printf "Part one result: %i\n" part_1;
  let part_2 = Day_2.Part_2.runner file in 
  Printf.printf "Part two Result: %i\n" part_2;
