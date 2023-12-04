let () =
  let cwd = Sys.getcwd () in
  Printf.printf "Part one\n";
  let file_1 = String.cat cwd "/files/day_1/dataset.txt" in
  Day_1.Part_1.runner file_1;
  Printf.printf "\nEnd Part one\n";
  let file_2 = String.cat cwd "/files/day_1/dataset.txt" in
  Day_1.Part_2.runner file_2;
