module CSS = struct
  let root = "mdc-checkbox"
  let native_control = BEM.add_element root "native-control"
  let background = BEM.add_element root "background"
  let checkmark = BEM.add_element root "checkmark"
  let checkmark_path = BEM.add_element root "checkmark-path"
  let mixedmark = BEM.add_element root "mixedmark"
  let disabled = BEM.add_modifier root "disabled"
  let upgraded = BEM.add_modifier root "upgraded"

  (* Animation *)
  let anim_checked_indeterminate = BEM.add_modifier root "anim-checked-indeterminate"
  let anim_checked_unchecked = BEM.add_modifier root "anim-checked-unchecked"
  let anim_indeterminate_checked = BEM.add_modifier root "anim-indeterminate-checked"
  let anim_indeterminate_unchecked = BEM.add_modifier root "anim-indeterminate-unchecked"
  let anim_unchecked_checked = BEM.add_modifier root "anim-unchecked-checked"
  let anim_unchecked_indeterminate = BEM.add_modifier root "anim-unchecked-indeterminate"
end

module Make (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(classes = []) ?attrs
        ?input_id ?(disabled = false) ?(checked = false) () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if disabled CSS.disabled
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs)
      [ input ~a:([ a_input_type `Checkbox
                  ; a_class [CSS.native_control]]
                  |> map_cons_option a_id input_id
                  |> cons_if disabled @@ a_disabled ()
                  |> cons_if checked @@ a_checked ()) ()
      ; div ~a:[a_class [CSS.background]]
          [ svg ~a:([ Svg.a_class [CSS.checkmark]
                    ; Svg.a_viewBox (0.0, 0.0, 24.0, 24.0)])
              [Svg.path ~a:([ Svg.a_class [CSS.checkmark_path]
                            ; Svg.a_fill `None
                            ; Svg.a_stroke (`Color ("white", None))
                            ; Svg.a_d "M1.73,12.91 8.1,19.28 22.79,4.59"])
                 []]
          ; div ~a:[a_class [CSS.mixedmark]] []]]
end
