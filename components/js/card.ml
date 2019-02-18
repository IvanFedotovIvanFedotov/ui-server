open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Card.Make(Xml)(Svg)(Html)

(* Sections *)

module Actions = struct

  module Buttons = struct

    class t ~(widgets : #Widget.t list) () =
      let (elt : Dom_html.element Js.t) =
        To_dom.of_element @@ Markup.Actions.Buttons.create () in
      object
        inherit Widget.t elt ~widgets () as super

        method! init () : unit =
          super#init ();
          List.iter (fun x ->
              x#add_class Markup.Actions.action_class;
              x#add_class Markup.Actions.action_button_class)
            widgets

      end

  end

  module Icons = struct

    class t ~(widgets : #Widget.t list) () =
      let (elt : Dom_html.element Js.t) =
        To_dom.of_element @@ Markup.Actions.Icons.create () in
      object
        inherit Widget.t elt ~widgets () as super

        method! init () : unit =
          super#init ();
          List.iter (fun x ->
              x#add_class Markup.Actions.action_class;
              x#add_class Markup.Actions.action_icon_class)
            widgets

      end

  end

  class t ~(widgets : #Widget.t list) () =
    let (elt : Dom_html.element Js.t) =
      To_dom.of_element @@ Markup.Actions.create () in
    object
      inherit Widget.t elt ~widgets ()
    end

end

module Primary = struct

  class overline (text : string) () =
    let (elt : Dom_html.element Js.t) =
      Markup.Primary.create_overline ~text ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  class title ?large (text : string) () =
    let (elt : Dom_html.element Js.t) =
      Markup.Primary.create_title ?large ~title:text ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  class subtitle (text : string) () =
    let (elt : Dom_html.element Js.t) =
      Markup.Primary.create_subtitle ~subtitle:text ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  class t ~(widgets : #Widget.t list) () =
    let (elt : Dom_html.element Js.t) =
      Markup.Primary.create ~children:(List.map Widget.to_markup widgets) ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

end

module Media = struct

  class t ~(widgets : #Widget.t list) () =
    let elt =
      Markup.Media.create ~children:(List.map Widget.to_markup widgets) ()
      |> To_dom.of_section in
    object
      val mutable widgets : Widget.t list = List.map Widget.coerce widgets

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        _widgets <- List.map Widget.coerce widgets
    end

end

class t ?(outlined = false)
        ?(form = false)
        ~(widgets : #Widget.t list)
        () =
  let tag = if form then Some Html.form else None in
  let elt = To_dom.of_element @@ Markup.create ?tag () in

  object(self)
    inherit Widget.t elt ~widgets () as super

    method! init () : unit =
      super#init ();
      self#set_outlined outlined

    method outlined : bool =
      super#has_class Markup.outlined_class

    method set_outlined (x : bool) : unit =
      super#toggle_class ~force:x Markup.outlined_class

  end