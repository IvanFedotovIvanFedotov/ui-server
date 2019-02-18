open Js_of_ocaml
open Tyxml_js
open Containers

module Markup = Components_tyxml.Button.Make(Xml)(Svg)(Html)

type style = [ `Raised | `Unelevated | `Stroked ]

class t ?typ ?style ?icon ?dense ?compact ?(ripple = true) ?label () =
  let icon' = Option.map Widget.to_markup icon in
  let (elt : Dom_html.buttonElement Js.t) =
    Markup.create ?button_type:typ ?button_style:style
      ?dense ?compact ?icon:icon' ?label ()
    |> To_dom.of_button in

  object

    val mutable _ripple : Ripple.t option = None

    inherit Widget.button_widget elt () as super

    method! init () : unit =
      super#init ();
      Option.iter (fun x -> x#add_class Markup.icon_class) icon;
      if ripple then
        let ripple = Ripple.attach super#root in
        _ripple <- Some ripple

    method! layout () : unit =
      super#layout ();
      Option.iter (fun r -> r#layout ()) _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun x -> x#destroy ()) _ripple;
      _ripple <- None

  end