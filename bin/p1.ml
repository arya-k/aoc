open Core

let () =
  In_channel.read_lines "p1.in"
  |> List.folding_map ~init:0 ~f:(fun sum line ->
         let sum' = match line with "" -> 0 | n -> sum + Int.of_string n in
         (sum', sum'))
  |> List.sort ~compare:Int.compare
  |> fun calories ->
  List.(take (rev calories) 3 |> sum (module Int) ~f:Fn.id)
  |> printf "Answer: %d\n"