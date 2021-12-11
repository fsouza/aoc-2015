let rec stdin () =
  let open Seq in
  try
    let line = read_line () in
    Cons (line, stdin)
  with End_of_file -> Nil

let nat =
  let open Seq in
  let rec nat' n () = Cons (n, nat' (n + 1)) in
  nat' 0
