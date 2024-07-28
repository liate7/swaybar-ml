open! ContainersLabels
open! Eio.Std
module Stream = Eio.Stream

let create ~full_format ~short_format update_interval_s clock tz =
  let update () =
    let mesg =
      let time =
        Eio.Time.now clock
        |> Timedesc.of_timestamp_float_s_exn ~tz_of_date_time:tz
      in
      Block.create_message
        ~short_text:(Timedesc.to_string ~format:short_format time)
        (Timedesc.to_string ~format:full_format time)
    in
    (mesg, ())
  in
  Block.Block { init = (); update; update_interval_s; update_reasons = [] }
