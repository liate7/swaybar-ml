open! Eio.Std
open! ContainersLabels
open Cohttp
open Cohttp_eio
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type forecast = {
  start_time : Timedesc.t;
  end_time : Timedesc.t;
  temperature : float;
  temperature_unit : string;
  short_forecast : string;
  percent_precipitation : float;
}

let ( $-> ) obj mem = Yojson.Safe.Util.member mem obj

let forecast_of_yojson json =
  let module Json = Yojson.Safe.Util in
  {
    start_time =
      json $-> "startTime" |> Json.to_string |> Timedesc.of_iso8601_exn;
    end_time = json $-> "endTime" |> Json.to_string |> Timedesc.of_iso8601_exn;
    temperature = json $-> "temperature" |> Json.to_number;
    temperature_unit = json $-> "temperatureUnit" |> Json.to_string;
    short_forecast = json $-> "shortForecast" |> Json.to_string;
    percent_precipitation =
      json $-> "probabilityOfPrecipitation" $-> "value" |> Json.to_number;
  }

let forecasts_to_show clock tz forecasts =
  forecasts
  |> List.drop_while ~f:(fun f ->
         Timedesc.compare_chrono_max f.end_time
           (Eio.Time.now clock
           |> Timedesc.of_timestamp_float_s_exn ~tz_of_date_time:tz)
         < 0)
  |> List.take 2

let full_message now next =
  Printf.sprintf "%d: %.0f°%s, %s (%.0f%%); %d: %.0f°%s, %s (%.0f%%)"
    (now.start_time |> Timedesc.hour)
    now.temperature now.temperature_unit now.short_forecast
    now.percent_precipitation
    (next.start_time |> Timedesc.hour)
    next.temperature next.temperature_unit
    (if String.(now.short_forecast = next.short_forecast) then "--"
     else next.short_forecast)
    now.percent_precipitation

let short_message forecast =
  Printf.sprintf "%d: %.0f°%s, %.0f%%"
    (forecast.start_time |> Timedesc.hour)
    forecast.temperature forecast.temperature_unit
    forecast.percent_precipitation

let create_message = function
  | now :: next :: _ ->
      Block.create_message
        ~short_text:(short_message now ^ "; " ^ short_message next)
      @@ full_message now next
  | _ -> Block.create_message "not enough forecasts"

let headers =
  Header.of_list
    [
      ("accept", "application/geo+json");
      ("user-agent", "Swaybar_ml (cohttp-eio)");
    ]

let create ~client update_interval clock tz uri =
  let update old =
    try
      Switch.run @@ fun sw ->
      match Client.get ~sw ~headers client uri with
      | { Response.status = `OK; _ }, body ->
          let json =
            Eio.Buf_read.(parse_exn take_all) ~max_size:Int.max_int body
            |> Yojson.Safe.from_string
          in
          let module Json = Yojson.Safe.Util in
          let forecasts =
            json $-> "properties" $-> "periods" |> [%of_yojson: forecast list]
          in
          (create_message (forecasts_to_show clock tz forecasts), forecasts)
      | { Response.status; _ }, _ ->
          traceln "Error getting weather info from %s: %d, %s"
            (Uri.to_string uri)
            (Code.code_of_status status)
            Code.(code_of_status status |> reason_phrase_of_code);
          (create_message (forecasts_to_show clock tz old), old)
    with
    | ( Eio.Io _ | Yojson.Json_error _ | End_of_file
      | Yojson.Safe.Util.Type_error _ | Failure _ ) as exn
    ->
      traceln "Error getting weather info: %s" @@ Printexc.to_string exn;
      (create_message (forecasts_to_show clock tz old), old)
  in
  Block.Block
    {
      init = [];
      update;
      update_interval_s = update_interval;
      update_reasons = [];
    }

let https ~certs_dir =
  let authenticator = X509_eio.authenticator (`Ca_dir certs_dir) in
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let hourly_forecast_uri forecast_office (x, y) =
  Uri.of_string
  @@ Printf.sprintf
       "https://api.weather.gov/gridpoints/%s/%d,%d/forecast/hourly"
       forecast_office x y
