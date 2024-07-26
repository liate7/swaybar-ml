open Eio.Std
module Stream = Eio.Stream
open! ContainersLabels
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type color = { red : int; green : int; blue : int; alpha : int option }

let color_to_string { red; green; blue; alpha } =
  assert (
    List.for_all
      ~f:(fun i -> i > 0 && i < 256)
      [ red; green; blue; Option.get_or ~default:0 alpha ]);
  match alpha with
  | Some alpha -> Printf.sprintf "#%x%x%x%x" red green blue alpha
  | None -> Printf.sprintf "#%x%x%x" red green blue

let yojson_of_color c = `String (color_to_string c)

type width = Pixels of int | Exemplar of string

let yojson_of_width = function Pixels p -> `Int p | Exemplar s -> `String s

type align = Left | Right | Center [@@deriving equal]

let yojson_of_align a =
  `String
    (match a with Left -> "left" | Right -> "right" | Center -> "center")

type markup = Pango | No_markup [@@deriving equal]

let yojson_of_markup m =
  `String (match m with Pango -> "pango" | No_markup -> "none")

type message = {
  full_text : string;
  short_text : string option; [@yojson.option]
  color : color option; [@yojson.option]
  background : color option; [@yojson.option]
  border : color option; [@yojson.option]
  border_top : int; [@default 1] [@yojson_drop_default ( = )]
  border_bottom : int; [@default 1] [@yojson_drop_default ( = )]
  border_left : int; [@default 1] [@yojson_drop_default ( = )]
  border_right : int; [@default 1] [@yojson_drop_default ( = )]
  min_width : width option; [@yojson.option]
  align : align; [@default Left] [@yojson_drop_default.equal]
  name : string option; [@yojson.option]
  instance : string option; [@yojson.option]
  urgent : bool; [@default false] [@yojson_drop_default Bool.equal]
  separator : bool; [@default false] [@yojson_drop_default Bool.equal]
  separator_block_width : int; [@default 9] [@yojson_drop_default ( = )]
  markup : markup; [@default No_markup] [@yojson_drop_default.equal]
}
[@@deriving yojson_of]

let create_message ?short_text ?color ?background ?border ?(border_top = 1)
    ?(border_bottom = 1) ?(border_left = 1) ?(border_right = 1) ?min_width
    ?(align = Left) ?name ?instance ?(urgent = false) ?(separator = false)
    ?(separator_block_width = 9) ?(markup = No_markup) full_text =
  {
    full_text;
    short_text;
    color;
    background;
    border;
    border_top;
    border_bottom;
    border_left;
    border_right;
    min_width;
    align;
    name;
    instance;
    urgent;
    separator;
    separator_block_width;
    markup;
  }

type 'state t = {
  init : 'state;
  update : 'state -> message * 'state;
  update_interval_s : float;
  update_reasons : (unit -> unit) List.t;
}

let run mono stream { init; update; update_interval_s; update_reasons } =
  let sleep () = Eio.Time.Mono.sleep mono update_interval_s in
  let rec loop state =
    let message, state = update state in
    Stream.add stream message;
    Fiber.any (sleep :: update_reasons);
    loop state
  in
  loop init
