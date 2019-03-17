module CSS = struct
  let root = "mdc-icon"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  module Font = struct

    let create ?(classes = []) ?attrs ~icon () : 'a elt =
      Html.i ~a:([a_class ("material-icons" :: CSS.root :: classes)]
                 <@> attrs)
        [txt icon]

  end

  module SVG = struct

    type size = [`S18 | `S24 | `S32 | `S48]

    let create_path ?(classes = []) ?attrs ?fill d () : 'a Svg.elt =
      Svg.path ~a:([Svg.a_class classes; Svg.a_d d]
                   |> map_cons_option (fun x -> Svg.a_fill @@ `Color (x, None))
                        fill
                   <@> attrs) []

    let create ?(classes = []) ?attrs ?(size = 24) paths () : 'a elt =
      let sz_fl = float_of_int size in
      svg ~a:Svg.([ a_class (CSS.root :: classes)
                  ; a_width (sz_fl, None)
                  ; a_height (sz_fl, None)
                  ; a_viewBox (0., 0., sz_fl, sz_fl)
                  ] <@> attrs)
        paths
  end
end
