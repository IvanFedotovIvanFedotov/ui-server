module CSS = struct
  (** Mandatory. Container for the outline's sides and notch. *)
  let root = "mdc-notched-outline"

  (** Mandatory. Element representing the leading side of the notched outline
      (before the notch). *)
  let leading = BEM.add_element root "leading"

  (** Mandatory. Element representing the notch. *)
  let notch = BEM.add_element root "notch"

  (** Mandatory. Element representing the trailing side of the notched outline
      (after the notch). *)
  let trailing = BEM.add_element root "trailing"

  (** Modifier class to open the notched outline. *)
  let notched = BEM.add_modifier root "notched"

  (** Modifier class to use when the floating label is empty or not used. *)
  let no_label = BEM.add_modifier root "no-label"

  let path = BEM.add_element root "path"
  let idle = BEM.add_element root "idle"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_path ?(classes = []) ?attrs () : 'a Svg.elt =
    let classes = CSS.path :: classes in
    Svg.(path ~a:([a_class classes] <@> attrs) [])

  let create_svg ?(classes = []) ?attrs path () : 'a elt =
    svg ~a:([Svg.a_class classes] <@> attrs) [path]

  let create_idle ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.idle :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create ?(classes = []) ?attrs svg () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs) [svg]

end
