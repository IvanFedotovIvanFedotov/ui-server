open Utils

module CSS = struct
  let root = "mdc-hexdump"
  let block = BEM.add_element root "block"
  let line_number = BEM.add_element root "line-number"
  let hex = BEM.add_element root "hex"
  let hex_empty = BEM.add_element root "hex-empty"
  let char = BEM.add_element root "char"
  let char_empty = BEM.add_element root "char-empty"

  let interactive = BEM.add_modifier root "interactive"
  let block_hex = BEM.add_modifier block "hex"
  let block_chars = BEM.add_modifier block "chars"
  let block_line_numbers = BEM.add_modifier block "line-numbers"
  let char_selected = BEM.add_modifier char "selected"
  let hex_selected = BEM.add_modifier hex "selected"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let create_block ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.block :: classes in
    pre ~a:([a_class classes] <@> attrs) []
end
