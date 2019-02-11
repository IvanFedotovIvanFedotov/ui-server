open Js_of_ocaml

val is_enabled : unit -> bool
val is_fullscreen : unit -> bool
val enter : #Dom.node Js.t -> unit
val cancel : unit -> unit
