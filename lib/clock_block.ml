open! ContainersLabels
open! Eio.Std
module Stream = Eio.Stream

let create update_interval_s clock tz format =
  let update () =
    let mesg =
      Eio.Time.now clock
      |> Timedesc.of_timestamp_float_s_exn ~tz_of_date_time:tz
      |> Timedesc.to_string ~format |> Block.create_message
    in
    (mesg, ())
  in
  Block.{ init = (); update; update_interval_s; update_reasons = [] }
