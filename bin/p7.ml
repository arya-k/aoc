open Core
open Angstrom

module Cmd = struct
  type path = Root | Out | In of string [@@deriving sexp_of]

  type t =
    | Cd of path
    | Ls
    | Dir of string
    | File of { size : int; name : string }
  [@@deriving sexp_of]

  (* primitives *)
  let ws = skip_while Char.is_whitespace
  let word = take_till Char.is_whitespace
  let num = take_while1 Char.is_digit >>| Int.of_string

  (* path *)
  let root = string "/" *> return Root
  let out = string ".." *> return Out
  let in_ = word >>| fun dir -> In dir

  (* commands *)
  let cmd ~name = string "$" *> ws *> string name <* ws
  let cd = cmd ~name:"cd" *> (root <|> out <|> in_) >>| fun path -> Cd path
  let ls = cmd ~name:"ls" *> return Ls
  let dir = string "dir" *> ws *> word >>| fun dir -> Dir dir
  let file = both num (ws *> word) >>| fun (size, name) -> File { size; name }

  (* top_level *)
  let parse =
    let parse_line = parse_string ~consume:All (cd <|> ls <|> dir <|> file) in
    List.map ~f:(Fn.compose Result.ok_or_failwith parse_line)
end

module FS = struct
  type _ t =
    | Dir : {
        name : string;
        mutable children : [ `dir | `file ] t list;
      }
        -> [> `dir ] t
    | File : { name : string; size : int } -> [> `file ] t
  [@@deriving sexp_of]

  let find ~name : [ `dir ] t -> [ `dir ] t = function
    | Dir { children; _ } ->
        let matches : type a. a t -> [ `dir ] t option = function
          | Dir { name = cand; _ } as child when String.equal cand name ->
              Some child
          | _ -> None
        in
        List.find_map_exn ~f:matches children

  let touch ~child : [ `dir ] t -> unit = function
    | Dir dir -> dir.children <- child :: dir.children

  let build instructions =
    List.fold instructions
      ~init:[ Dir { name = "/"; children = [] } ]
      ~f:(fun stk cmd ->
        let top = List.hd_exn stk in
        match cmd with
        | Cmd.Cd Root -> [ List.last_exn stk ]
        | Cd Out -> List.drop stk 1
        | Cd (In name) -> find ~name top :: stk
        | Ls -> stk
        | Dir name ->
            touch top ~child:(Dir { name; children = [] });
            stk
        | File { size; name } ->
            touch top ~child:(File { name; size });
            stk)
    |> List.last_exn
    |> (function Dir root -> Dir root : [ `dir ] t -> [ `dir | `file ] t)
end

let part2 root =
  let sizes =
    let rec helper = function
      | FS.Dir { children; _ } ->
          let children = List.map children ~f:helper in
          let size = List.sum (module Int) children ~f:fst in
          (size, size :: List.concat (List.map children ~f:snd))
      | FS.File { size; _ } -> (size, [])
    in
    helper root |> snd
  in
  let needed = List.hd_exn sizes - 40000000 in
  List.filter sizes ~f:(fun size -> size >= needed)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let () =
  let fs = In_channel.read_lines "p7.in" |> Cmd.parse |> FS.build in
  part2 fs |> printf "\n\nAnswer: %d"
