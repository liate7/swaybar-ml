open! ContainersLabels
open! Eio.Std
module Stdenv = Eio.Stdenv
open Swaybar_ml
open Cohttp_eio

let minutes n = n *. 60.

let virga_main _lwt_tok env =
  let client =
    let certs_dir =
      Eio.Path.(
        env#fs
        / try Unix.getenv "SSL_CERT_DIR" with Not_found -> "/etc/ssl/certs")
    in
    Client.make
      ~https:(Some (Weather_block.https ~certs_dir |> Result.get_exn))
      env#net
  and uri = Weather_block.hourly_forecast_uri "RNK" (99, 76)
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
      Clock_block.create 0.5 clock tz ~full_format ~short_format;
    ]

let nostalgia_main lwt_tok env =
  let client =
    let certs_dir =
      Eio.Path.(
        env#fs
        / try Unix.getenv "SSL_CERT_DIR" with Not_found -> "/etc/ssl/certs")
    in
    Client.make
      ~https:(Some (Weather_block.https ~certs_dir |> Result.get_exn))
      env#net
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
      Battery_block.create lwt_tok display_bat ~update_interval:5.
        ~urgent_under:Int64.(19L * 60L);
      Clock_block.create 0.5 clock tz ~full_format ~short_format;
    ]

let () =
  Eio_linux.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun tok ->
  (match
     Eio.Path.(load (Eio.Stdenv.fs env / "/etc" / "hostname")) |> String.trim
   with
  | "virga" -> virga_main
  | "nostalgia-for-inf-0" -> nostalgia_main
  | host -> failwith (Printf.sprintf "Host \"%s\" not supported" host))
    tok env
