open Core

module Move = struct
  type t = { q : int; f : int; t : int } [@@deriving sexp_of]

  let of_string line : t =
    match String.split line ~on:' ' with
    | [ "move"; q; "from"; f; "to"; t ] ->
        let q, f, t = Tuple3.map (q, f, t) ~f:Int.of_string in
        { q; f = pred f; t = pred t }
    | _ -> failwith "invalid move"
end

module State = struct
  type t = char list list [@@deriving sexp_of]

  let of_lines lines =
    List.map lines ~f:String.to_list
    |> List.transpose_exn
    |> List.map ~f:(List.filter ~f:Char.is_alpha)
    |> List.filter ~f:(Fn.non List.is_empty)

  (** [apply t move] is the state after moving containers as described. *)
  let apply t (move : Move.t) =
    let delta, from' = List.split_n (List.nth_exn t move.f) move.q in
    let to' = delta @ List.nth_exn t move.t in
    List.mapi t ~f:(fun i x ->
        if i = move.f then from' else if i = move.t then to' else x)

  (** [tops t] is the top of each stack, ' ' otherwise, in a single string *)
  let tops t =
    List.map t ~f:(function [] -> ' ' | hd :: _ -> hd) |> String.of_char_list
end

let () =
  let lines = In_channel.read_lines "p5.in" in
  let state, moves = List.split_while lines ~f:(Fn.non String.is_empty) in
  List.drop moves 1 |> List.map ~f:Move.of_string
  |> List.fold ~init:(State.of_lines state) ~f:State.apply
  |> State.tops |> printf "Answer: %s"
