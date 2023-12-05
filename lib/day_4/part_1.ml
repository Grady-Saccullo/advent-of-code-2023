(**
Capture groups are as follows:
1: Card number
2: Playing numbers
3: Empty
4: Card numbers
5: Empty
 *)
let card_regex =
  Str.regexp {|Card[ ]+\([0-9]*\)\:\(\([ ]+[0-9]*\)+\)|\(\([ ]+[0-9]*\)+\)|}

let split_regex = Str.regexp {|[ ]+|}

let line_parser line =
  if Str.string_match card_regex line 0 then
    let playing_numbers = Str.matched_group 2 line in
    let card_numbers = Str.matched_group 4 line in
    (* TODO: figure out why i have to split on a new line vs piping *)
    let playing_numbers = Str.split split_regex playing_numbers in
    let card_numbers = Str.split split_regex card_numbers in
    let num =
      List.filter (fun x -> List.mem x playing_numbers) card_numbers
      |> List.length
    in
    if num == 0 then 0 else Float.pow 2.0 (Int.to_float (num - 1)) |> Int.of_float
  else 0

let runner file =
  let lines = Shared.File.read_lines file in
  List.map line_parser lines |> List.fold_left (fun acc x -> acc + x) 0
