let is_increasing a b c =
  let a = Char.code a in
  let b = Char.code b in
  let c = Char.code c in
  b - a = 1 && c - b = 1

let is_allowed_char = function
  | 'i' | 'o' | 'l' -> false
  | _ -> true

let is_valid password =
  let length = Array.length password in
  let rec is_valid' pairs increasing idx =
    if idx = length then
      let pairs =
        if password.(idx - 2) = password.(idx - 1) then pairs + 1 else pairs
      in
      pairs > 1 && increasing
    else
      let first, second, ch =
        (password.(idx - 2), password.(idx - 1), password.(idx))
      in
      if not (is_allowed_char ch) then false
      else if Char.equal first second && not (Char.equal second ch) then
        is_valid' (pairs + 1) increasing (idx + 1)
      else if (not increasing) && is_increasing first second ch then
        is_valid' pairs true (idx + 1)
      else is_valid' pairs increasing (idx + 1)
  in
  length > 2
  && is_allowed_char password.(0)
  && is_allowed_char password.(1)
  && is_valid' 0 false 2

let incr_ch ch =
  if Char.equal ch 'z' then ('a', true)
  else
    let delta =
      match ch with
      | 'i' | 'o' | 'l' -> 2
      | _ -> 1
    in
    (ch |> Char.code |> ( + ) delta |> Char.chr, false)

let incr password =
  let rec incr' idx =
    if idx = -1 then ()
    else
      let ch, wrapped = incr_ch password.(idx) in
      password.(idx) <- ch;
      if wrapped then incr' (idx - 1)
  in
  incr' (Array.length password - 1)

let next password =
  let rec next' password =
    incr password;
    if is_valid password then password |> Array.to_seq |> String.of_seq
    else next' password
  in
  next' (password |> String.to_seq |> Array.of_seq)

let () = read_line () |> next |> print_endline
