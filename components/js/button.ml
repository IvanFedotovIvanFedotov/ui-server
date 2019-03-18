open Js_of_ocaml
open Utils

include Components_tyxml.Button
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t ?(ripple = true) ?on_click (elt : #Dom_html.element Js.t) () =
object
  val mutable _ripple : Ripple.t option = None
  val mutable _click_listener : unit Lwt.t option = None

  inherit Widget.button_widget elt () as super

  method! init () : unit =
    super#init ();
    if ripple then
      let ripple = Ripple.attach super#root in
      _ripple <- Some ripple

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    match on_click with
    | None -> ()
    | Some f ->
       let listener = super#listen_click_lwt (fun e _ -> f e) in
       _click_listener <- Some listener

  method! layout () : unit =
    super#layout ();
    Option.iter (fun r -> r#layout ()) _ripple

  method! destroy () : unit =
    super#destroy ();
    Option.iter (fun x -> x#destroy ()) _ripple;
    _ripple <- None
end

let make ?typ ?appearance ?icon ?dense ?ripple ?label ?on_click () : t =
  let icon = match icon with
    | None -> None
    | Some (i : #Widget.t) ->
       i#add_class CSS.icon;
       Some (Widget.to_markup i) in
  let (elt : Dom_html.buttonElement Js.t) =
    Tyxml_js.To_dom.of_button
    @@ Markup.create ?button_type:typ ?appearance ?dense ?icon ?label () in
  new t ?ripple ?on_click elt ()

let attach ?ripple ?on_click (elt : #Dom_html.element Js.t) : t =
  new t ?ripple ?on_click elt ()
