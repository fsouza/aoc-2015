let divisors_of n =
  let open Seq in
  let rec divisors_of' i () =
    if i * i > n then Nil
    else if n mod i = 0 then
      let other = n / i in
      let v = if other > i then i + other else i in
      Cons (v, divisors_of' (i + 1))
    else divisors_of' (i + 1) ()
  in
  divisors_of' 1

let calc_presents house_number =
  house_number |> divisors_of |> Seq.map (( * ) 10) |> Seq.fold_left ( + ) 0

let find_answer target =
  let rec find_answer' house_number =
    if calc_presents house_number >= target then house_number
    else find_answer' (house_number + 1)
  in
  find_answer' 1

let () = read_line () |> int_of_string |> find_answer |> Printf.printf "%d\n"
