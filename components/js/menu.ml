open Js_of_ocaml
open Utils

include Components_tyxml.Menu
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let elements_key_allowed_in = ["input"; "button"; "textarea"; "select"; "a"]

class t (elt : Dom_html.element Js.t) () =
object

  inherit Menu_surface.t elt () as super

  val mutable close_animation_end_timer : Utils.timer_id option = None

  method! init () : unit =
    super#init ()

  method! destroy () : unit =
    super#destroy ();
    Option.iter Utils.clear_timeout close_animation_end_timer;
    close_animation_end_timer <- None

  method! private handle_keydown (e : Dom_html.keyboardEvent Js.t) : unit =
    match Events.Key.of_event e with
    | `Tab -> super#close ()
    | `Arrow_up | `Arrow_down ->
       Dom.preventDefault e
    | _ -> ()
end
