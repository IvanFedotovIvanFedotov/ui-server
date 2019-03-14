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

end

module Key = struct

  type t =
    [ `Enter
    | `Escape
    | `Space
    | `End
    | `Home
    | `Arrow_left
    | `Arrow_right
    | `Arrow_up
    | `Arrow_down
    | `Delete
    | `Page_up
    | `Page_down
    | `Char of char
    | `Digit of int (* integers in range 0 - 9 *)
    | `Unknown
    ]

  let of_event (e : Dom_html.keyboardEvent Js.t) : t =
    let key = match Js.Optdef.to_option e##.key with
      | None -> None
      | Some x -> Some (Js.to_string x) in
    (match key, e##.keyCode with
     | Some "Enter", _ | _, 13 -> `Enter
     | Some "Escape", _ | _, 27 -> `Escape
     | Some "Space", _ | _, 32 -> `Space
     | Some "End", _ | _, 35 -> `End
     | Some "Home", _ | _, 36 -> `Home
     | Some "ArrowLeft", _ | _, 37 -> `Arrow_left
     | Some "ArrowRight", _ | _, 39 -> `Arrow_right
     | Some "ArrowUp", _ | _, 38 -> `Arrow_up
     | Some "ArrowDown", _ | _, 40 -> `Arrow_down
     | Some "Delete", _ | _, 46 -> `Delete
     | Some "PageUp", _ | _, 33 -> `Page_up
     | Some "PageDown", _ | _, 34 -> `Page_down
     | _, x when x >= 48 && x <= 57 ->
        let d = int_of_string @@ Char.escaped @@ Char.chr x in
        `Digit d
     | _, x when x >= 65 && x <= 90 ->
        let char = Char.chr x in
        let char =
          if Js.to_bool e##.shiftKey
          then Char.uppercase_ascii char
          else char in
        `Char char
     | _ -> `Unknown)

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
