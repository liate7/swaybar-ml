open! ContainersLabels
open Lwt

let ( let* ) = ( >>= )
let ( let+ ) = ( >|= )
let ( and+ ) = Lwt.Infix.Let_syntax.both

let time_to_string secs =
  let open Timedesc.Span in
  make ~s:secs () |> For_human.to_string ~format:"{hours:X:}{mins:0X}"

(* TODO: make this reactive? *)
let create (_ : Lwt_eio.Token.t) ~update_interval battery ~urgent_under =
  let update () =
    let percentage, state, to_empty, to_full =
      Lwt_eio.run_lwt @@ fun () ->
      let+ percentage = UPower_device.percentage battery |> OBus_property.get
      and+ state = UPower_device.state battery |> OBus_property.get
      and+ to_empty = UPower_device.time_to_empty battery |> OBus_property.get
      and+ to_full = UPower_device.time_to_full battery |> OBus_property.get in
      (percentage, state, to_empty, to_full)
    in
    let in_parens =
      match state with
      | `Charging | `Pending_charge ->
          time_to_string to_full |> Printf.sprintf "CHR, %s"
      | `Discharging | `Pending_discharge ->
          time_to_string to_empty |> Printf.sprintf "DIS, %s"
      | `Fully_charged -> "FULL"
      | `Empty -> "EMPTY"
      | `Unknown -> "UNK"
    in
    let full_text = Printf.sprintf "⚡%3.0f%% (%s)" percentage in_parens
    and urgent =
      match state with
      | `Discharging | `Pending_discharge ->
          Int64.(to_empty <> 0L && to_empty < urgent_under)
      | _ -> false
    in
    (Block.create_message ~urgent full_text, ())
  in
  Block.Block
    {
      init = ();
      update;
      update_interval_s = update_interval;
      update_reasons = [];
    }

let battery_at_path (_ : Lwt_eio.Token.t) path =
  Lwt_eio.run_lwt @@ fun () ->
  let ( let+ ) = Lwt.( >|= ) and ( >|= ) = Lwt.( >|= ) in
  let+ peer = UPower.daemon () >|= UPower.to_peer in
  OBus_proxy.make ~peer ~path |> UPower_device.of_proxy
