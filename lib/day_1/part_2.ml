let matcher k = match k with '0' .. '9' -> Some k | _ -> None

let match_word_to_number w =
  match w with
  | "one" -> Some "1"
  | "two" -> Some "2"
  | "three" -> Some "3"
  | "four" -> Some "4"
  | "five" -> Some "5"
  | "six" -> Some "6"
  | "seven" -> Some "7"
  | "eight" -> Some "8"
  | "nine" -> Some "9"
  | _ -> None

let get_line_value line =
  let out = ref [] in
  let offset = ref 0 in

  while !offset < String.length line do
    (match matcher line.[!offset] with
    | Some k ->
        let v = String.make 1 k in
        out := !out @ [ v ]
    | None ->

        if String.length line - !offset >= 3 then
          let word = String.sub line !offset 3 in
          let num = match_word_to_number word in
          if num <> None then (
            let v = Option.get num in
            offset := !offset + 1;
            out := !out @ [ v ])
          else if String.length line - !offset >= 4 then
            let word = String.sub line !offset 4 in
            let num = match_word_to_number word in

            if num <> None then (
              let v = Option.get num in
              offset := !offset + 2;
              out := !out @ [ v ])
            else if String.length line - !offset >= 5 then
              let word = String.sub line !offset 5 in
              let num = match_word_to_number word in

              if num <> None then (
                let v = Option.get num in
                offset := !offset + 3;
                out := !out @ [ v ]));

    offset := !offset + 1
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
