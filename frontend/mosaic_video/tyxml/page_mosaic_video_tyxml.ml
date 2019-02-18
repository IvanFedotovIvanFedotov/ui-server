open Components_tyxml

module Player = Player

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  module CSS = struct
    let root = "mosaic"
    let side_sheet_icon = CSS.add_element root "side-sheet-icon"
  end

end

