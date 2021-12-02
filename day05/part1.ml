let is_vowel = function
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let is_forbidden = function
  | 'a', 'b' | 'c', 'd' | 'p', 'q' | 'x', 'y' -> true
  | _ -> false

let result vowels repeated_letters = vowels >= 3 && repeated_letters

let is_nice str =
  let len = String.length str in
  let rec loop (vowels, repeated_letters) idx =
    if idx = len then result vowels repeated_letters
    else
      let ch = str.[idx] in
      let vowels = if is_vowel ch then vowels + 1 else vowels in
      if idx > 0 then
        let prev = str.[idx - 1] in
        let repeated_letters = repeated_letters || Char.equal ch prev in
        if is_forbidden (str.[idx - 1], str.[idx]) then false
        else loop (vowels, repeated_letters) (idx + 1)
      else loop (vowels, repeated_letters) (idx + 1)
  in
  loop (0, false) 0

let () =
  Aoc.stdin
  |> Seq.filter is_nice
  |> Seq.fold_left (fun acc _ -> acc + 1) 0
  |> Printf.printf "%d\n"
