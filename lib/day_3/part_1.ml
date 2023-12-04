type point = { idx : int; row : int }
type sumbol_touch_points = { upper : point; lower : point }
type location = First | Middle | Last

let is_symbol ch =
  match ch with '.' -> None | '0' .. '9' -> None | v -> Some v

let create_symbol_touch_points idx cur_row r_type c_type =
  match (r_type, c_type) with
  | First, First ->
      {
        upper = { idx; row = cur_row };
        lower = { idx = idx + 1; row = cur_row + 1 };
      }
  | First, Middle ->
      {
        upper = { idx = idx - 1; row = cur_row };
        lower = { idx = idx + 1; row = cur_row + 1 };
      }
  | First, Last ->
      {
        upper = { idx = idx - 1; row = cur_row };
        lower = { idx; row = cur_row + 1 };
      }
  | Middle, First ->
      {
        upper = { idx; row = cur_row - 1 };
        lower = { idx = idx + 1; row = cur_row + 1 };
      }
  | Middle, Middle ->
      {
        upper = { idx = idx - 1; row = cur_row - 1 };
        lower = { idx = idx + 1; row = cur_row + 1 };
      }
  | Middle, Last ->
      {
        upper = { idx = idx - 1; row = cur_row - 1 };
        lower = { idx; row = cur_row + 1 };
      }
  | Last, First ->
      {
        upper = { idx; row = cur_row - 1 };
        lower = { idx = idx + 1; row = cur_row };
      }
  | Last, Middle ->
      {
        upper = { idx = idx - 1; row = cur_row - 1 };
        lower = { idx = idx + 1; row = cur_row };
      }
  | Last, Last ->
      {
        upper = { idx = idx - 1; row = cur_row - 1 };
        lower = { idx; row = cur_row };
      }

let runner file =
  let lines = Shared.File.read_lines file in
  lines
