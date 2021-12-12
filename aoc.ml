open Seq

let rec stdin () =
  try
    let line = read_line () in
    Cons (line, stdin)
  with End_of_file -> Nil

let nat =
  let rec nat' n () = Cons (n, nat' (n + 1)) in
  nat' 0

let rec zip seq1 seq2 () =
  match (seq1 (), seq2 ()) with
  | Cons (v1, seq1), Cons (v2, seq2) -> Cons ((v1, v2), zip seq1 seq2)
  | _ -> Nil
