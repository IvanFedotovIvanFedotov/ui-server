open Js_of_ocaml

include Components_tyxml.Card
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

(* Sections *)

module Actions = struct
  module Buttons = struct
    class t ?(widgets : #Button.t list option) (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t ?widgets elt () as super

      method! init () : unit =
        super#init ()

      method buttons : Button.t list =
        []
    end

    let make ?(widgets = []) () : t =
      (* XXX maybe the user should take care of adding classes explicitly? *)
      List.iter (fun x ->
          x#add_class CSS.action;
          x#add_class CSS.action_button)
        widgets;
      let children = List.map Widget.to_markup widgets in
      let (elt : Dom_html.element Js.t) =
        Tyxml_js.To_dom.of_element
        @@ Markup.create_action_buttons children () in
      new t elt ()

    let attach (elt : #Dom_html.element Js.t) : t =
      new t (Element.coerce elt) ()
  end

  module Icons = struct
    class t ?widgets (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t ?widgets elt () as super

      method! init () : unit =
        super#init ()
    end

    let make ?(widgets = []) () : t =
      (* XXX maybe the user should take care of adding classes explicitly? *)
      List.iter (fun x ->
          x#add_class CSS.action;
          x#add_class CSS.action_icon)
        widgets;
      let children = List.map Widget.to_markup widgets in
      let (elt : Dom_html.element Js.t) =
        Tyxml_js.To_dom.of_element
        @@ Markup.create_action_icons children () in
      new t elt ()

    let attach (elt : #Dom_html.element Js.t) : t =
      new t (Element.coerce elt) ()
  end

  class t ?widgets (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t ?widgets elt ()
  end

  let make ?(widgets = []) () : t =
    let children = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_actions children () in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()
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
  class t ?widgets (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t ?widgets elt () as super

    method! init () : unit =
      super#init ()
  end

  let make ?(widgets = []) () : t =
    let children = List.map Widget.to_markup widgets in
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_section
      @@ Markup.create_media children () in
    new t ~widgets elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()
end

class t ?widgets (elt : Dom_html.element Js.t) () =
object
  inherit Widget.t ?widgets elt () as super

  method outlined : bool =
    super#has_class CSS.outlined

  method set_outlined (x : bool) : unit =
    super#toggle_class ~force:x CSS.outlined
end

let make ?outlined ?(tag = Tyxml_js.Html.div) ?widgets
      () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?outlined ~tag [] () in
  new t ?widgets elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
