module CSS = struct
  let root = "mdc-floating-label"
  let float_above = BEM.add_modifier root "float-above"
  let shake = BEM.add_modifier root "shake"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(classes = []) ?attrs ?for_ text () : 'a elt =
    let classes = CSS.root :: classes in
    label ~a:([a_class classes]
              |> map_cons_option a_label_for for_
              <@> attrs) [txt text]
end
