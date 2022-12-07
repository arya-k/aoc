open Core

module Interval = struct
  (** overlaps [t1], [t2] is whether the intervals overlap. *)
  let overlaps (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

  let of_string s =
    match String.split s ~on:'-' with
    | [ s; e ] -> (int_of_string s, int_of_string e)
    | _ -> failwith "invalid interval"
end

let () =
  In_channel.read_lines "p4.in"
  |> List.map ~f:(fun line ->
         match String.split line ~on:',' with
         | [ i1; i2 ] -> (Interval.of_string i1, Interval.of_string i2)
         | _ -> failwith "invalid input")
  |> List.filter ~f:(Tuple2.uncurry Interval.overlaps)
  |> List.length
  |> Printf.printf "Answer: %d\n"
