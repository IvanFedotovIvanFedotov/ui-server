open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    (** Mandatory. Container for the snackbar elements. *)
    let root = "mdc-snackbar"

    (** Mandatory. Snackbar surface. *)
    let surface = CSS.add_element root "surface"

    (** Mandatory. Message text. *)
    let label = CSS.add_element root "label"

    (** Optonal. Wraps the action button/icon elements, if present. *)
    let actions = CSS.add_element root "actions"

    (** Optional. The action button. *)
    let action = CSS.add_element root "action"

    (** Optional. The dismiss ("X") icon. *)
    let dismiss = CSS.add_element root "dismiss"

    (** Optional. Applied automatically when the snackbar is in the process
        of animating open. *)
    let opening = CSS.add_modifier root "opening"

    (** Optional. Indicates that the snackbar is visible. *)
    let open_ = CSS.add_modifier root "open"

    (** Optional. Applied automatically when the snackbar is in the process
        of anumating closed. *)
    let closing = CSS.add_modifier root "closing"

    (** Optional. Positions the snackbar on the leading edge of the screen
        (left in LTR, right in RTL) instead of centered. *)
    let leading = CSS.add_modifier root "leading"

    (** Optional. Positions the action button/icon below the label instead
        of alongside it. *)
    let stacked = CSS.add_modifier root "stacked"

  end

  let create_label ?(classes = []) ?attrs (text : 'a elt) () : 'a elt =
    let classes = CSS.label :: classes in
    div ~a:([ a_class classes
            ; a_aria "live" ["polite"]
            ; a_role ["status"]]
            <@> attrs)
      [text]

  let create_actions ?(classes = []) ?attrs
        (actions : 'a elt list) () : 'a elt =
    let classes = CSS.actions :: classes in
    div ~a:([a_class classes] <@> attrs) actions

  let create_surface ?(classes = []) ?attrs ?actions ~label () : 'a elt =
    let classes = CSS.surface :: classes in
    div ~a:([a_class classes] <@> attrs)
      (match actions with
       | None -> [label]
       | Some actions -> [label; actions])

  let create ?(classes = []) ?attrs ?(leading = false) ?(stacked = false)
        ~(surface : 'a elt) () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if leading CSS.leading
      |> cons_if stacked CSS.stacked
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs) [surface]

end
