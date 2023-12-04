type color = Red | Green | Blue
type row = { mutable red : int; mutable green : int; mutable blue : int }

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
  let out = { red = 1; blue = 1; green = 1 } in
  let _ =
    List.nth chunks 1 |> String.trim |> String.split_on_char ';'
    |> List.map String.trim
    |> List.map (fun x -> String.split_on_char ',' x)
    |> List.map (List.map String.trim)
    |> List.map (List.map parse_color)
    |> List.map (List.filter (fun (_, c) -> c <> None))
    |> List.iter (fun x ->
           List.iter
             (fun y ->
               match y with
               | a, Some Red -> if a > out.red then out.red <- a
               | a, Some Green -> if a > out.green then out.green <- a
               | a, Some Blue -> if a > out.blue then out.blue <- a
               | _ -> ())
             x)
  in
  out 

let runner file =
  let lines = Shared.File.read_lines file in
  List.map line_parser lines
  |> List.fold_left (fun acc x -> acc + (x.red * x.green * x.blue)) 0
