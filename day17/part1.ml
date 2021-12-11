let solve liters containers =
  let rec solve' target containers =
    if target = 0 then 1
    else if target < 0 then 0
    else
      match containers with
      | [] -> 0
      | hd :: tl -> solve' (target - hd) tl + solve' target tl
  in
  solve' liters containers

let () =
  let liters = Sys.argv.(1) |> int_of_string in
  Aoc.stdin
  |> Seq.map int_of_string
  |> List.of_seq
  |> solve liters
  |> Printf.printf "%d\n"
