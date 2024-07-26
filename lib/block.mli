open Eio.Std

type color = { red : int; green : int; blue : int; alpha : int option }
type width = Pixels of int | Exemplar of string
type align = Left | Right | Center
type markup = Pango | No_markup

type message = {
  full_text : string;
  short_text : string option;
  color : color option;
  background : color option;
  border : color option;
  border_top : int;
  border_bottom : int;
  border_left : int;
  border_right : int;
  min_width : width option;
  align : align;
  name : string option;
  instance : string option;
  urgent : bool;
  separator : bool;
  separator_block_width : int;
  markup : markup;
}
[@@deriving yojson_of]

val create_message :
  ?short_text:string ->
  ?color:color ->
  ?background:color ->
  ?border:color ->
  ?border_top:int ->
  ?border_bottom:int ->
  ?border_left:int ->
  ?border_right:int ->
  ?min_width:width ->
  ?align:align ->
  ?name:string ->
  ?instance:string ->
  ?urgent:bool ->
  ?separator:bool ->
  ?separator_block_width:int ->
  ?markup:markup ->
  string ->
  message

type 'state t = {
  init : 'state;
  update : 'state -> message * 'state;
  update_interval_s : float;
  update_reasons : (unit -> unit) List.t;
}

val run : [> Eio.Time.Mono.ty ] r -> message Eio.Stream.t -> 'state t -> 'a
