let () =
  let cwd = Sys.getcwd () in
  Printf.printf "Part one\n";
  let file_1 = String.cat cwd "/files/day_2/dataset.txt" in
  let v = Day_2.Part_1.runner file_1 in 
  Printf.printf "Result: %s\n" v;
