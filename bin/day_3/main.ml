
let () =
  let cwd = Sys.getcwd () in
  let file = String.cat cwd "/files/day_3/dataset.txt" in
  let part_1 = Day_3.Part_1.runner file in
  Printf.printf "Part one result: %i\n" part_1;
  Printf.printf "Part two result: not finished";
