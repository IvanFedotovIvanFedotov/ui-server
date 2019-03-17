module CSS = struct
  let root = "mdc-divider"
  let inset = BEM.add_modifier root "inset"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(classes = []) ?attrs ?(inset = false) () : 'a elt =
    let classes =
      classes
      |> cons_if inset CSS.inset
      |> List.cons CSS.root in
    hr ~a:([a_class classes] <@> attrs) ()

end
