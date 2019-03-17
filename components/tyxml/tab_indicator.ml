open Utils

module CSS = struct
  (** Mandatory. Contains the tab indicator content. *)
  let root = "mdc-tab-indicator"

  (** Mandatory. Denotes the tab indicator content. *)
  let content = BEM.add_element root "content"

  (** Optional. Visually activates the indicator. *)
  let active = BEM.add_modifier root "active"

  (** Optional. Sets up the tab indicator to fade in on activation
      and fade out on deactivation. *)
  let fade = BEM.add_modifier root "fade"

  (** Optional. Denotes an underline tab indicator. *)
  let content_underline = BEM.add_modifier content "underline"

  (** Optional. Denotes an icon tab indicator. *)
  let content_icon = BEM.add_modifier content "icon"
end

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let create_content ?(classes = []) ?attrs ?(icon = false) () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if icon CSS.content_icon
      |> cons_if (not icon) CSS.content_underline
      |> List.cons CSS.content in
    span ~a:([a_class classes] <@> attrs) []

  let create ?(classes = []) ?attrs ?(active = false)
        ?(fade = false) content () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if fade CSS.fade
      |> cons_if active CSS.active
      |> List.cons CSS.root in
    span ~a:([a_class classes] <@> attrs) [content]

end
