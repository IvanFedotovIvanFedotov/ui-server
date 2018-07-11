open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  type size = [ `S18 | `S24 | `S32 | `S48 ]

  let base_class     = "mdc-icon"
  let button_class   = CSS.add_modifier base_class "button"
  let disabled_class = CSS.add_modifier base_class "disabled"

  module Font = struct
    let create ?(classes=[]) ?attrs ~icon () =
      Html.i ~a:([a_class ("material-icons" :: base_class :: classes)] <@> attrs) [pcdata icon]
  end

  module SVG = struct

    module Path = struct

      open Icon_svg

      type t =
        | Tv | Chevron_down | Chevron_up | Lock | Clock_outline | Download
        | Auto_fix

      let to_string = function
        | Auto_fix      -> Action.auto_fix
        | Tv            -> tv
        | Chevron_down  -> Navigation.chevron_down
        | Chevron_up    -> Navigation.chevron_up
        | Lock          -> lock
        | Clock_outline -> clock_outline
        | Download      -> download

    end

    let create_path ?(classes=[]) ?attrs ?fill d () =
      Svg.path ~a:([ Svg.a_class classes
                   ; Svg.a_d (Path.to_string d) ]
                   |> map_cons_option (fun x -> Svg.a_fill @@ `Color (x, None))
                        fill
                   <@> attrs) []

    let create ?(classes=[]) ?attrs ?(size=24) paths () =
      let sz    = Printf.sprintf "width:%dpx;height:%dpx" size size in
      let sz_fl = float_of_int size in
      svg ~a:([ Svg.a_class (base_class :: classes)
              ; Svg.a_style sz
              ; Svg.a_viewBox (0.,0.,sz_fl,sz_fl)
              ] <@> attrs)
        paths

  end

end
