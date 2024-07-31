open! ContainersLabels
open! Eio.Std
module Net = Eio.Net
open Result.Infix

type play_state = Play | Stop | Pause
type single_state = On | Off | Once
type playing_song_info = { elapsed : float; duration : float; song_id : int }

type status = {
  state : play_state;
  single : single_state;
  repeat : bool;
  random : bool;
  consume : bool;
  playing : playing_song_info option;
}

let mpd_bool_of_string = function
  | "1" -> Some true
  | "0" -> Some false
  | _ -> None

let read_status ?(finished = "OK") buf =
  let state = ref None
  and single = ref None
  and repeat = ref None
  and random = ref None
  and consume = ref None
  and elapsed = ref None
  and duration = ref None
  and song_id = ref None in
  let build () =
    let+ state = !state |> Option.to_result (`Missing "state")
    and+ single = !single |> Option.to_result (`Missing "single")
    and+ repeat = !repeat |> Option.to_result (`Missing "repeat")
    and+ random = !random |> Option.to_result (`Missing "random")
    and+ consume = !consume |> Option.to_result (`Missing "consume") in
    let playing =
      let open Option.Infix in
      let+ song_id = !song_id
      and+ elapsed = !elapsed
      and+ duration = !duration in
      { elapsed; duration; song_id }
    in
    { state; single; repeat; random; consume; playing }
  in
  let rec loop buf =
    let line = Eio.Buf_read.line buf in
    if String.(line = finished) then build ()
    else
      match Stringext.cut line ~on:" " with
      | Some ("ACK", err) -> Error (`Mpd_error err)
      | Some ("state:", "play") ->
          state := Some Play;
          loop buf
      | Some ("state:", "stop") ->
          state := Some Stop;
          loop buf
      | Some ("state:", "pause") ->
          state := Some Pause;
          loop buf
      | Some ("elapsed:", secs) ->
          elapsed := Float.of_string_opt secs;
          loop buf
      | Some ("duration:", secs) ->
          duration := Float.of_string_opt secs;
          loop buf
      | Some ("single:", "0") ->
          single := Some Off;
          loop buf
      | Some ("single:", "1") ->
          single := Some On;
          loop buf
      | Some ("single:", "oneshot") ->
          single := Some Once;
          loop buf
      | Some ("repeat:", state) ->
          repeat := mpd_bool_of_string state;
          loop buf
      | Some ("random:", state) ->
          random := mpd_bool_of_string state;
          loop buf
      | Some ("consume:", state) ->
          consume := mpd_bool_of_string state;
          loop buf
      | Some ("songid:", id) ->
          song_id := Int.of_string id;
          loop buf
      | None | Some _ -> loop buf
  in
  loop buf

module String_map = Map.Make (String)

type song = {
  file : string;
  album : string option;
  title : string option;
  grouping : string option;
  composers : string list;
  artists : string list;
  performers : string list;
  other : string list String_map.t;
}

let read_song ?(finished = "OK") buf =
  let deduplicate_option name = function
    | Some [ album ] -> Ok (Some album)
    | None | Some [] -> Ok None
    | _ -> Error (`Duplicate name)
  in
  let map_pop key t =
    let* opt = String_map.get key t |> deduplicate_option key in
    match opt with
    | Some value -> Ok (value, String_map.remove key t)
    | None -> Error (`Missing key)
  in
  let option_to_list = function None -> [] | Some l -> l in
  let build map =
    let* file, map = map_pop "file" map in
    let+ album = String_map.get "album" map |> deduplicate_option "album"
    and+ title = String_map.get "title" map |> deduplicate_option "title"
    and+ grouping =
      String_map.get "grouping" map |> deduplicate_option "grouping"
    in
    {
      file;
      album;
      title;
      grouping;
      composers = String_map.get "composer" map |> option_to_list;
      artists = String_map.get "artist" map |> option_to_list;
      performers = String_map.get "performer" map |> option_to_list;
      other =
        List.fold_left ~init:map
          [ "album"; "title"; "grouping"; "composer"; "artist"; "performer" ]
          ~f:(fun map key -> String_map.remove key map);
    }
  in
  let rec loop map =
    let line = Eio.Buf_read.line buf in
    if String.(line = finished) then build map
    else
      match Stringext.cut line ~on:" " with
      | Some ("ACK", err) -> Error (`Mpd_error err)
      | Some (key, value) ->
          let key =
            String.replace ~which:`Right ~sub:":" ~by:"" key
            |> String.lowercase_ascii
          in
          map
          |> String_map.update key (function
               | None -> Some [ value ]
               | Some lst -> Some (value :: lst))
          |> loop
      | None -> loop map
  in
  loop String_map.empty

let read_idle ?(finished = "OK") buf =
  let rec loop changes =
    let line = Eio.Buf_read.line buf in
    if String.(line = finished) then Ok changes
    else
      match Stringext.cut line ~on:" " with
      | Some ("ACK", err) -> Error (`Mpd_error err)
      | Some ("changed:", change) -> loop (change :: changes)
      | _ -> loop changes
  in
  loop []

let command name f sock buf =
  Eio.Flow.copy_string (name ^ "\n") sock;
  f buf

let with_mpd_socket net addr f =
  Switch.run @@ fun sw ->
  let sock = Eio.Net.connect ~sw net addr in
  let buf = Eio.Buf_read.(of_flow ~max_size:Int.max_int sock) in
  (match Eio.Buf_read.line buf |> String.split ~by:" " with
  | [ "OK"; "MPD"; _version ] -> ()
  | _ -> assert false);
  let ret = f sock buf in
  Eio.Flow.shutdown sock `Send;
  ret

let song_to_string song =
  Printf.sprintf "[%s] %s%s"
    Option.(
      List.head_opt song.composers
      |> or_ ~else_:song.album |> get_or ~default:"")
    (song.grouping
    |> Option.map (fun s -> s ^ ": ")
    |> Option.get_or ~default:"")
    (song.title |> Option.get_or ~default:song.file)

let play_state_to_string = function Play -> "▶" | Pause -> "⏸" | Stop -> "⏹"

let single_state_to_string = function On -> "s" | Once -> "1" | Off -> ""

let error_to_string = function
  | `Missing field -> [%string "%{field} is missing."]
  | `Mpd_error err -> [%string "MPD returned error: %{err}"]
  | `Duplicate field -> [%string "Duplicate fields %{field}."]

let progress_bar ~current ~total width =
  (* Based on guix's progress bar function *)
  let start = "▕" and stop = "▏" and filled = "█" in
  let steps = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; filled |] in
  let width = Int.max 3 (width - 2) in
  let scaled = current *. Float.of_int width /. total in
  let filled = floor scaled |> Int.of_float |> String.repeat filled
  and empty = width - Int.of_float (floor scaled) - 1 |> String.repeat " "
  and intermediate =
    steps.(Int.of_float
           @@ Float.round
                ((scaled -. floor scaled)
                *. Float.of_int (Array.length steps - 1)))
  in
  let ret = Printf.sprintf "%s%s%s%s%s" start filled intermediate empty stop in
  let utf8 = Utf8_string.of_string_exn ret in
  assert (
    Utf8_string.n_chars utf8 = width + 2
    ||
    (traceln "%s: %f %f %d" ret current total @@ Utf8_string.n_chars utf8;
     false));
  ret

let do_block_update net addr =
  let fetch () =
    with_mpd_socket net addr @@ fun sock buf ->
    let* status = command "status" read_status sock buf in
    match status.playing with
    | Some playing ->
        let+ current_song = command "currentsong" read_song sock buf in
        (status, Some (playing, current_song))
    | None -> Ok (status, None)
  in
  match fetch () with
  | Ok (status, Some (playing, current_song)) ->
      let full_text =
        Printf.sprintf "%s %s %.0f%s%s"
          (song_to_string current_song)
          (play_state_to_string status.state)
          (playing.duration /. 60.)
          (progress_bar 10 ~current:playing.elapsed ~total:playing.duration)
          (single_state_to_string status.single)
      and short_text =
        Printf.sprintf "%s %.1f/%.1f %s"
          (current_song.title |> Option.get_or ~default:current_song.file)
          (playing.elapsed /. 60.) (playing.duration /. 60.)
          (play_state_to_string status.state)
      in
      (Block.create_message ~short_text full_text, ())
  | Ok (status, None) ->
      let full_text =
        Printf.sprintf "%s %s"
          (play_state_to_string status.state)
          (single_state_to_string status.single)
      in
      (Block.create_message full_text, ())
  | Error err ->
      traceln "Error getting MPD info: %s" @@ error_to_string err;
      (Block.create_message "Error getting MPD info", ())
  | exception ((Eio.Io _ | End_of_file | Failure _) as exn) ->
      traceln "Error getting MPD info: %s" @@ Printexc.to_string exn;
      (Block.create_message "Error getting MPD info", ())

let poll_idle net addr =
  try with_mpd_socket net addr @@ command "idle" read_idle |> ignore
  with (Eio.Io _ | End_of_file | Failure _) as exn ->
    traceln "Error in MPD idle: %s" @@ Printexc.to_string exn

let create update_interval_s net addr =
  Block.Block
    {
      init = ();
      update = (fun () -> do_block_update net addr);
      update_interval_s;
      update_reasons = [ (fun () -> poll_idle net addr) ];
    }
