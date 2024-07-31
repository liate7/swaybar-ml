open! ContainersLabels
open! Eio.Std
module Stdenv = Eio.Stdenv
open Swaybar_ml
open Cohttp_eio

let https ~certs_dir =
  let authenticator = X509_eio.authenticator (`Ca_dir certs_dir) in
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let minutes n = n *. 60.

let main lwt_tok env =
  let client =
    let certs_dir = Eio.Path.(env#fs / Unix.getenv "SSL_CERT_DIR") in
    Client.make ~https:(Some (https ~certs_dir)) env#net
  and uri =
    Uri.of_string "https://api.weather.gov/gridpoints/RNK/99,76/forecast/hourly"
  and display_bat =
    Lwt_eio.run_lwt @@ fun () ->
    let ( let+ ) = Lwt.( >|= ) and ( >|= ) = Lwt.( >|= ) in
    let+ peer = UPower.daemon () >|= UPower.to_peer in
    OBus_proxy.make ~peer
      ~path:[ "org"; "freedesktop"; "UPower"; "devices"; "DisplayDevice" ]
    |> UPower_device.of_proxy
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
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun tok -> main tok env
