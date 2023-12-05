let matcher k = match k with '0' .. '9' -> Some k | _ -> None

let get_line_value line =
  let out = ref [] in
  for i = 0 to String.length line - 1 do
    match matcher line.[i] with
    | Some k ->
        let v = String.make 1 k in
        out := !out @ [ v ]
    | None -> ()
  done;

  if List.length !out = 0 then 0
  else if List.length !out = 1 then
    let v = Printf.sprintf "%s%s" (List.hd !out) (List.hd !out) in
    int_of_string v
  else
    let first = List.hd !out in
    let last = List.hd (List.rev !out) in
    let v = Printf.sprintf "%s%s" first last in
    int_of_string v

let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t ->
      let v = get_line_value h in
      v + sum t

let runner file_location =
  let lines = Shared.File.read_lines file_location in
  sum lines
