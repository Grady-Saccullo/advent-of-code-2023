let seeds_regex = Str.regexp {|seeds:\(\([ ]+[0-9]+\)+\)|}
let split_regex = Str.regexp {|[ ]+|}

type map_data_line = { source_range : int * int; dest_start : int }

type map_header_line = string * string
(** map_header_line is a tuple of (Source, Dest) *)

type line_type =
  | MapDataLine of map_data_line
  | MapHeaderLine of map_header_line

type map_data = { dest_name : string; data : map_data_line list }

let seeds_parser line =
  if Str.string_match seeds_regex line 0 then
    let seeds_str = Str.matched_group 1 line in
    let seeds = Str.split split_regex seeds_str |> List.map int_of_string in
    Some seeds
  else None

let map_regex = Str.regexp {|\([0-9]*\)[ ]+\([0-9]*\)[ ]+\([0-9]*\)|}
let map_header_regex = Str.regexp {|\([a-z]+\)-to-\([a-z]+\)[ ]+map:|}

let map_parser line =
  if Str.string_match map_regex line 0 then
    let dest = Str.matched_group 1 line |> int_of_string in
    let src = Str.matched_group 2 line |> int_of_string in
    let range = Str.matched_group 3 line |> int_of_string in
    Some
      (* Probably need to account for range being 0...*)
      (MapDataLine { source_range = (src, src + range - 1); dest_start = dest })
  else if Str.string_match map_header_regex line 0 then
    let src = Str.matched_group 1 line in
    let dest = Str.matched_group 2 line in
    Some (MapHeaderLine (src, dest))
  else None

let compare_map_data_line_source a b =
  let a = fst a.source_range in
  let b = fst b.source_range in
  if a < b then -1 else if a > b then 1 else 0

let final_dest = "location"

let rec find_next_location id src_name maps =
  let map = Hashtbl.find maps src_name in
  let rec find_next_location_helper id map =
    match map.data with
    | [] -> id
    | hd :: tl ->
        let rng_start, rng_end = hd.source_range in
        if rng_start <= id && id <= rng_end then
          let offset = id - rng_start in
          hd.dest_start + offset
        else if rng_start > id then id
        else find_next_location_helper id { map with data = tl }
  in
  let d = find_next_location_helper id map in
  if map.dest_name = final_dest then d
  else find_next_location d map.dest_name maps

let runner file =
  let lines = Shared.File.read_lines file in
  let seeds = seeds_parser (List.nth lines 0) in
  let maps = Hashtbl.create 7 in
  let dest_name = ref None in
  let src_name = ref None in
  let current_map_data = ref [] in
  for i = 1 to List.length lines - 1 do
    let line = List.nth lines i in
    match map_parser line with
    | Some (MapDataLine mapper) ->
        current_map_data := mapper :: !current_map_data
    | Some (MapHeaderLine (src, dest)) ->
        dest_name := Some dest;
        src_name := Some src
    | None ->
        if !dest_name == None || !src_name == None then ()
        else
          let data =
            {
              dest_name = Option.get !dest_name;
              data = List.sort compare_map_data_line_source !current_map_data;
            }
          in
          let key = Option.get !src_name in
          Hashtbl.add maps key data;
          dest_name := None;
          src_name := None;
          current_map_data := []
  done;

  let data =
    {
      dest_name = Option.get !dest_name;
      data = List.sort compare_map_data_line_source !current_map_data;
    }
  in
  let key = Option.get !src_name in
  Hashtbl.add maps key data;

  List.map (fun id -> find_next_location id "seed" maps) (Option.get seeds)
  |> List.fold_left (fun acc id -> if acc == 0 || id < acc then id else acc) 0
