type color = Red | Green | Blue

let color_regex = Str.regexp {|\([0-9]*\) \([a-z]*\)|}
let game_regex = Str.regexp {|\Game \([0-9]*\)|}

let match_color color =
  match color with
  | "red" -> Some Red
  | "green" -> Some Green
  | "blue" -> Some Blue
  | _ -> None

let parse_color value =
  if Str.string_match color_regex value 0 then
    let a = Str.matched_group 1 value |> int_of_string in
    let color = Str.matched_group 2 value |> match_color in
    (a, color)
  else (0, None)

let parse_game value =
  if Str.string_match game_regex value 0 then
    Str.matched_group 1 value |> int_of_string
  else 0

let is_color_amt_valid (a, c) =
  match c with
  | Some Red -> a <= 12
  | Some Green -> a <= 13
  | Some Blue -> a <= 14
  | None -> false

let is_possible_row values = List.for_all is_color_amt_valid values

let line_parser line =
  let chunks = line |> String.split_on_char ':' in
  let key = List.nth chunks 0 |> parse_game in
  let valid =
    List.nth chunks 1 |> String.trim |> String.split_on_char ';'
    |> List.map String.trim
    |> List.map (fun x -> String.split_on_char ',' x)
    |> List.map (List.map String.trim)
    |> List.map (List.map parse_color)
    |> List.for_all is_possible_row
  in
  (key, valid)

let runner file =
  let lines = Shared.File.read_lines file in
  List.filter (fun (_, v) -> v) (List.map line_parser lines)
  |> List.filter (fun (_, v) -> v)
  |> List.map (fun (k, _) -> k)
  |> List.fold_left ( + ) 0 |> string_of_int
