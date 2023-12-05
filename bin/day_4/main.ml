let () =
  let cwd = Sys.getcwd () in
  let file = String.cat cwd "/files/day_4/dataset.txt" in
  let v = Day_4.Part_1.runner file in
  Printf.printf "Part one result: %i\n" v;
  let res = Day_4.Part_2.runner file in 
  Printf.printf "Part two Result: %i\n" res;
