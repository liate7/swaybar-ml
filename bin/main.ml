open! ContainersLabels
open! Eio.Std
module Stdenv = Eio.Stdenv
open Swaybar_ml

let main env =
  let clock = Stdenv.clock env in
  Core.start (Stdenv.mono_clock env) (Stdenv.stdout env)
    [
      Clock_block.create 0.5 clock
        (Timedesc.Time_zone.make_exn "America/New_York")
        "{year}-{mon:0X}-{day:0X} ({wday:Xxx}, \
         UTC{tzoff-sign}{tzoff-hour:0X}{tzoff-min:0X}) \
         {hour:0X}:{min:0X}:{sec:0X}";
    ]

let () = Eio_linux.run main
