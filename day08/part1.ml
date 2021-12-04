let parsed_length str =
  let len = String.length str in
  let rec parse' acc idx =
    if idx = len - 1 then acc
    else
      let ch = str.[idx] in
      if Char.equal ch '\\' then
        let delta =
          match str.[idx + 1] with
          | '"' | '\\' -> 2
          | 'x' -> 4
          | _ -> 1
        in
        parse' (acc + 1) (idx + delta)
      else parse' (acc + 1) (idx + 1)
  in
  parse' 0 1

let lengths line = (String.length line, parsed_length line)

let () =
  Aoc.stdin
  |> Seq.map lengths
  |> Seq.fold_left
       (fun (total_raw, total_parsed) (raw, parsed) ->
         (total_raw + raw, total_parsed + parsed))
       (0, 0)
  |> (fun (total_raw, total_parsed) -> total_raw - total_parsed)
  |> Printf.printf "%d\n"
