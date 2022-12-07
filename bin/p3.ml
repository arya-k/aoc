open Core

let () =
  let process group =
    List.map group ~f:(String.fold ~init:Char.Set.empty ~f:Char.Set.add)
    |> List.reduce_exn ~f:Char.Set.inter
    |> Char.Set.choose_exn
    |> function
    | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
    | c -> Char.to_int c - Char.to_int 'A' + 27
  in
  In_channel.read_lines "p3.in"
  |> List.chunks_of ~length:3
  |> List.sum (module Int) ~f:process
  |> Printf.printf "Answer: %d\n"
