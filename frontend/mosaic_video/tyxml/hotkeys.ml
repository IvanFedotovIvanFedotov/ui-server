open Components_tyxml
open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    let root = "hotkeys"
    let section = CSS.add_element root "section"
    let section_title = CSS.add_element root "section-title"
    let option = CSS.add_element root "option"
    let label = CSS.add_element root "label"
    let hotkey = CSS.add_element root "hotkey"
  end

  let create_hotkey ?(classes = []) ?attrs hotkey () : 'a elt =
    let classes = CSS.hotkey :: classes in
    div ~a:([a_class classes] <@> attrs) [txt hotkey]

  let create_label ?(classes = []) ?attrs label () : 'a elt =
    let classes = CSS.label :: classes in
    div ~a:([a_class classes] <@> attrs) [txt label]

  let create_option ?(classes = []) ?attrs ~label ~hotkey () : 'a elt =
    let classes = CSS.option :: classes in
    div ~a:([a_class classes] <@> attrs) [label; hotkey]

  let create_section_title ?(classes = []) ?attrs title () : 'a elt =
    let classes = CSS.section_title :: classes in
    div ~a:([a_class classes] <@> attrs) [txt title]

  let create_section ?(classes = []) ?attrs ~title ~options () : 'a elt =
    let classes = CSS.section :: classes in
    div ~a:([a_class classes] <@> attrs)
      [ title
      ; div options
      ]

  let create ?(classes = []) ?attrs ~sections () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs) sections

end
