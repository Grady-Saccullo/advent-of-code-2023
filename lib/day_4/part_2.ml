(*
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
    num
  else 0

let runner file =
  let lines = Shared.File.read_lines file in
  let len = List.length lines in
  let cards = Array.make len 0 in
  for i = 0 to len - 1 do
    let line = List.nth lines i in
    let num = line_parser line in
    cards.(i) <- cards.(i) + 1;
    if num > 0 then (
      for j = i + 1 to i + num do
        cards.(j) <- cards.(j) + cards.(i)
      done)
  done;
  Array.to_list cards |> List.fold_left ( + ) 0
