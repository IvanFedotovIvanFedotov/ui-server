module CSS = struct
  let root = "mdc-expansion-panel"
  let expanded = BEM.add_modifier root "expanded"
  let panel_wrapper = BEM.add_element root "panel-wrapper"
  let panel = BEM.add_element root "panel"
  let primary = BEM.add_element root "primary"
  let primary_summary = BEM.add_element root "summary"
  let primary_details = BEM.add_element root "details"
  let primary_heading = BEM.add_element root "heading"
  let actions = BEM.add_element root "actions"
  let action = BEM.add_element root "action"
  let icon = BEM.add_element root "icon"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  module Divider = Divider.Make(Xml)(Svg)(Html)
  module Icon = Icon.Make(Xml)(Svg)(Html)

  let create_primary ?(classes = []) ?attrs
        ?(heading_details = []) ?(details = []) ~title () : 'a elt =
    div ~a:([ a_class (CSS.primary :: classes)
            ; a_tabindex 0 ] <@> attrs)
      [ div ~a:[a_class [CSS.primary_summary]]
          [ div ~a:[a_class [CSS.primary_heading]]
              ((match heading_details with
                | [] -> None
                | l -> Some (div ~a:[a_class [CSS.primary_details]] l))
               |> (fun x -> cons_option x [])
               |> List.cons (txt title))
          ; div ~a:[a_class [CSS.primary_details]] details
          ]
      ; div ~a:[a_class [CSS.icon]; a_tabindex (-1)]
          (* FIXME font icon *)
          [Icon.Font.create ~icon:"expand_more" ()]
      ]

  let create_actions ?(classes = []) ?attrs ~actions () : 'a elt =
    let classes = CSS.actions :: classes in
    div ~a:([a_class classes] <@> attrs) actions

  let create_panel ?(classes = []) ?attrs ~content () : 'a elt =
    let classes = CSS.panel :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes=[]) ?attrs ?actions ~primary ~panel () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs)
      [ primary
      ; div ~a:[a_class [CSS.panel_wrapper]]
          (match actions with
           | None -> [panel]
           | Some x -> panel :: Divider.create () :: x :: [])]
end
