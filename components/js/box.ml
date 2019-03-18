open Js_of_ocaml

include Components_tyxml.Box
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type direction =
  [ `Row
  | `Column
  ]

class t ?widgets (elt : Dom_html.element Js.t) () =
object
  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    match widgets with
    | None -> ()
    | Some w ->
       (* XXX do we need this? *)
       _widgets <- w

  method set_direction : direction -> unit = function
    | `Row ->
       super#remove_class CSS.vertical;
       super#add_class CSS.horizontal
    | `Column ->
       super#remove_class CSS.horizontal;
       super#add_class CSS.vertical

  method set_wrap (x : wrap) : unit =
    super#add_class @@ CSS.wrap x;

  method set_justify_content x =
    super#add_class @@ CSS.justify_content x

  method set_align_items x =
    super#add_class @@ CSS.align_items x

  method set_align_content x =
    super#add_class @@ CSS.align_content x
end

let make ?tag ?justify_content ?align_items ?align_content
      ?wrap ~direction ~(widgets : #Widget.t list) : t =
  let content = List.map Widget.to_markup widgets in
  let vertical = match direction with
    | `Row -> false
    | `Column -> true in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?tag ?justify_content ?align_items ?align_content ?wrap
         ~vertical ~content () in
  new t ~widgets elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (elt :> Dom_html.element Js.t) ()
