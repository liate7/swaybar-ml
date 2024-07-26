open! ContainersLabels
open! Eio.Std
module Stream = Eio.Stream
module Buf_write = Eio.Buf_write

let go ~sw:_ out streams =
  let row = Fiber.List.map Stream.take streams in
  let streams =
    streams |> List.mapi ~f:(fun i stream () -> (i, Stream.take stream))
  in
  let rec loop row =
    let idx, new_block = Fiber.any streams in
    let row = row |> List.set_at_idx idx new_block in
    Buf_write.with_flow out (fun out ->
        `List (List.map ~f:Block.yojson_of_message row)
        |> Yojson.Safe.to_string ~suf:",\n"
        |> Buf_write.string out);
    loop row
  in
  loop row

let start mono out blocks =
  let streams = List.map blocks ~f:(fun _ -> Stream.create 1) in
  Switch.run @@ fun sw ->
  List.iter2
    ~f:(fun stream block ->
      Fiber.fork_daemon ~sw (fun () -> Block.run mono stream block))
    streams blocks;
  Buf_write.with_flow out (fun out ->
      `Assoc [ ("version", `Int 1) ]
      |> Yojson.Safe.to_string ~suf:"\n"
      |> Buf_write.string out;
      Buf_write.string out "[\n");
  go ~sw out streams
