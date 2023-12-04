let () =
  let cwd = Sys.getcwd () in
  let file_1 = String.cat cwd "/files/day_2/dataset.txt" in
  let v = Day_2.Part_1.runner file_1 in
  Printf.printf "Part one result: %s\n" v;
  let file_2 = String.cat cwd "/files/day_2/dataset.txt" in
  let res = Day_2.Part_2.runner file_2 in 
  Printf.printf "Part two Result: %i\n" res;
