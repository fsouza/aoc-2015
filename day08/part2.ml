open StdLabels

let encoded_length =
  String.fold_left ~init:2 ~f:(fun acc -> function
    | '\\' | '"' -> acc + 2
    | _ -> acc + 1)

let lengths line = (encoded_length line, String.length line)

let () =
  Aoc.stdin
  |> Seq.map lengths
  |> Seq.fold_left
       (fun (total_raw, total_parsed) (raw, parsed) ->
         (total_raw + raw, total_parsed + parsed))
       (0, 0)
  |> (fun (total_raw, total_parsed) -> total_raw - total_parsed)
  |> Printf.printf "%d\n"
