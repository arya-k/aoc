open Core

let () =
  let unique = Fn.non (List.contains_dup ~compare:Char.compare) in
  let line = In_channel.read_lines "p6.in" |> List.hd_exn |> String.to_list in
  List.findi_exn line ~f:(fun i _ -> List.sub line ~pos:i ~len:14 |> unique)
  |> Tuple2.get1 |> ( + ) 14 |> printf "\n\nAnswer: %d"
