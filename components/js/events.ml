open Js_of_ocaml

class type event = Dom_html.event

module Typ = struct

  include Dom_events.Typ

  type 'a t = 'a typ

  class type wheelEvent =
    object
      inherit Dom_html.mouseEvent
      method deltaX : int
      method deltaY : int
      method deltaZ : int
      method deltaMode : int
    end

  let (wheel : wheelEvent Js.t typ) =
    make "wheel"

  let (fullscreenchange : event Js.t typ) =
    make "fullscreenchange"

end

include (Dom_events : module type of Dom_events with module Typ := Typ)

let listen_lwt :
      'a. ?cancel_handler:bool -> ?use_capture:bool ->
      #Dom.node Js.t -> (#event as 'a) Js.t Typ.t ->
      ('a Js.t -> unit Lwt.t -> unit Lwt.t) ->
      unit Lwt.t =
  fun ?cancel_handler ?use_capture node x f ->
  Lwt_js_events.seq_loop (Lwt_js_events.make_event x)
    ?cancel_handler ?use_capture node f
