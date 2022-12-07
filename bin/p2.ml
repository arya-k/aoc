open Core

module Outcome = struct
  type t = Lose | Tie | Win

  let parse_exn = function
    | "X" -> Lose
    | "Y" -> Tie
    | "Z" -> Win
    | _ -> failwith "invalid outcome"

  let value = function Lose -> 0 | Tie -> 3 | Win -> 6
end

module Shape = struct
  type t = Rock | Paper | Scissors

  let parse_exn = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "invalid shape"

  let value = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let response t ~outcome =
    match (t, outcome) with
    | Rock, Outcome.Lose -> Scissors
    | Rock, Win -> Paper
    | Paper, Lose -> Rock
    | Paper, Win -> Scissors
    | Scissors, Lose -> Paper
    | Scissors, Win -> Rock
    | shape, Tie -> shape
end

(* Part 1 *)
let () =
  In_channel.read_lines "p2.in"
  |> List.map ~f:(fun line ->
         let shape, outcome = String.lsplit2_exn line ~on:' ' in
         let shape = Shape.parse_exn shape in
         let outcome = Outcome.parse_exn outcome in
         let response = Shape.response shape ~outcome in
         Outcome.value outcome + Shape.value response)
  |> List.sum (module Int) ~f:Fn.id
  |> Printf.printf "Answer: %d\n"
