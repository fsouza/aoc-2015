open StdLabels
open MoreLabels

let line_re =
  Re.compile
    (Re.Pcre.re
       {|^(\w+):\s+capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$|})

type ingredient = {
  name : string;
  capacity : int;
  durability : int;
  flavor : int;
  texture : int;
  calories : int;
}

module Recipe = Map.Make (struct
  type t = ingredient

  let compare ingredient1 ingredient2 =
    String.compare ingredient1.name ingredient2.name
end)

let parse line =
  line
  |> Re.exec_opt line_re
  |> Option.map Re.Group.all
  |> (Fun.flip Option.bind) (function
       | [| _; ingredient; capacity; durability; flavor; texture; calories |] ->
           Some
             {
               name = ingredient;
               capacity = int_of_string capacity;
               durability = int_of_string durability;
               flavor = int_of_string flavor;
               texture = int_of_string texture;
               calories = int_of_string calories;
             }
       | _ -> None)

let score ingredients =
  ingredients
  |> Seq.fold_left
       (fun (capacity_acc, durability_acc, flavor_acc, texture_acc)
            ({ capacity; durability; flavor; texture; _ }, n) ->
         ( capacity_acc + (capacity * n),
           durability_acc + (durability * n),
           flavor_acc + (flavor * n),
           texture_acc + (texture * n) ))
       (0, 0, 0, 0)
  |> fun (c, d, f, t) ->
  if c < 0 || d < 0 || f < 0 || t < 0 then 0 else c * d * f * t

let recipe_score recipe = recipe |> Recipe.to_seq |> score

let make_recipe ingredients amounts =
  amounts
  |> List.map ~f:(fun amount -> if amount < 0 then 0 else amount)
  |> List.combine ingredients
  |> List.to_seq
  |> Recipe.of_seq

(* Note: this hard codes the number of ingredients and runs a brute-force
   algorithm, we should do better, using some constraints solving library could
   be useful :) *)
let run ingredients =
  let max = 100 in
  let largest = ref 0 in
  for first = 0 to max do
    for second = 0 to max - first do
      for third = 0 to max - first - second do
        let fourth = max - first - second - third in
        let recipe = make_recipe ingredients [ first; second; third; fourth ] in
        let score = recipe_score recipe in
        if score > !largest then largest := score
      done
    done
  done;
  !largest

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> List.of_seq
  |> run
  |> Printf.printf "%d\n"
