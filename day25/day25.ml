let multiplier = 252533
let divisor = 33554393
let next row col = if row = 1 then (col + 1, 1) else (row - 1, col + 1)

let find_value v target_row target_col =
  let rec find_value' v row col =
    if row = target_row && col = target_col then v
    else
      let row, col = next row col in
      find_value' (v * multiplier mod divisor) row col
  in
  find_value' v 1 1

let () =
  let row = read_int () in
  let col = read_int () in
  let start_value = 20151125 in
  Printf.printf "%d\n" @@ find_value start_value row col
