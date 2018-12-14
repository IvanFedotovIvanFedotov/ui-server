open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-dialog"
  let container_class = CSS.add_element base_class "container"
  let surface_class = CSS.add_element base_class "surface"
  let scrim_class = CSS.add_element base_class "scrim"
  let animating_class = CSS.add_modifier base_class "animating"
  let scroll_lock_class = "mdc-dialog-scroll-lock"
  let scrollable_class = CSS.add_modifier base_class "scrollable"
  let open_class = CSS.add_modifier base_class "open"

  module Title = struct

    let _class = CSS.add_element base_class "title"

    let create ?(classes = []) ?attrs ~title () : 'a elt =
      h2 ~a:([a_class (_class :: classes)] <@> attrs) [txt title]

  end

  module Content = struct

    let _class = CSS.add_element base_class "content"

    let create ?(classes = []) ?attrs ~content () : 'a elt =
      section ~a:([a_class (_class :: classes)] <@> attrs) content

  end

  module Actions = struct

    let _class = CSS.add_element base_class "actions"
    let button_class = CSS.add_element _class "button"
    let default_class = CSS.add_modifier button_class "default"
    let accept_button_class = CSS.add_modifier button_class "accept"
    let cancel_button_class = CSS.add_modifier button_class "cancel"

    let create ?(classes = []) ?attrs ~children () : 'a elt =
      footer ~a:([ a_class (_class :: classes)] <@> attrs) children

  end

  let create_container ?(classes = []) ?attrs surface () : 'a elt =
    div ~a:([a_class (container_class :: classes)] <@> attrs) [surface]

  let create_surface ?(classes = []) ?attrs content () : 'a elt =
    div ~a:([a_class (surface_class :: classes)] <@> attrs) content

  let create_scrim ?(classes = []) ?attrs () : 'a elt =
    div ~a:([a_class (scrim_class :: classes)] <@> attrs) []

  let create ?(classes = []) ?attrs ?label_id ?description_id
        ?(scrollable = false) ~scrim ~container () : 'a elt =
    let aria n v = a_aria n [v] in
    div ~a:([ a_class (classes
                       |> cons_if scrollable scrollable_class
                       |> List.cons base_class)
            ; a_role ["alertdialog"]
            ; a_aria "modal" ["true"]]
            |> map_cons_option (aria "labelledby") label_id
            |> map_cons_option (aria "describedby") description_id
            <@> attrs)
      [container; scrim]

end
