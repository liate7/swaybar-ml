open! ContainersLabels
open! Eio.Std
module Stdenv = Eio.Stdenv
open Swaybar_ml
open Cohttp_eio

let minutes n = n *. 60.

let main lwt_tok env =
  let client =
    let certs_dir = Eio.Path.(env#fs / Unix.getenv "SSL_CERT_DIR") in
    Client.make ~https:(Some (Weather_block.https ~certs_dir)) env#net
  and uri = Weather_block.hourly_forecast_uri "RNK" (99, 76)
  and display_bat =
    Battery_block.battery_at_path lwt_tok
      [ "org"; "freedesktop"; "UPower"; "devices"; "DisplayDevice" ]
  and clock = env#clock
  and tz = Timedesc.Time_zone.make_exn "America/New_York"
  and full_format =
    "{year}-{mon:0X}-{day:0X} ({wday:Xxx}, \
     UTC{tzoff-sign}{tzoff-hour:0X}{tzoff-min:0X}) {hour:0X}:{min:0X}:{sec:0X}"
  and short_format = "--{mon:0X}-{day:0X} {hour:0X}:{min:0X}:{sec:0X}" in
  Swaybar.start env#mono_clock env#stdout
    [
      Mpd_block.create 15. env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 6600));
      Weather_block.create ~client (minutes 5.) clock tz uri;
      Battery_block.create lwt_tok ~update_interval:5. ~urgent_under:19L
        display_bat;
      Clock_block.create 0.5 clock tz ~full_format ~short_format;
    ]

let () =
  Eio_linux.run @@ fun env ->
  Mirage_crypto_rng_eio.run
    (module Mirage_crypto_rng.Fortuna)
    (env
      :> < clock : [> ] Eio.Time.clock
         ; mono_clock : [> ] Eio.Time.Mono.t
         ; secure_random : [> ] Eio.Flow.source >)
  @@ fun () ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun tok -> main tok env
