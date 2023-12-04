
let () =
  let cwd = Sys.getcwd () in
  let file_1 = String.cat cwd "/files/day_3/dataset.txt" in
  let v = Day_3.Part_1.runner file_1 in
  Printf.printf "Part one result: %i\n" !v;
