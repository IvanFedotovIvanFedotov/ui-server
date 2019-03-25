module CSS = struct
  (** Mandatory. *)
  let root = "mdc-text-field"

  (** Styles the text field as an outlined text field. *)
  let outlined = BEM.add_modifier root "outlined"

  (** Styles the text field as a full width text field. *)
  let fullwidth = BEM.add_modifier root "fullwidth"

  (** Indicates the text field is a <textarea>. *)
  let textarea = BEM.add_modifier root "textarea"

  (** Styles the text field as a disabled text field. *)
  let disabled = BEM.add_modifier root "disabled"

  let invalid = BEM.add_modifier root "invalid"

  (** Styles the text field as a text field with a leading icon. *)
  let with_leading_icon = BEM.add_modifier root "with-leading-icon"

  (** Styles the text field as a text field with a trailing icon. *)
  let with_trailing_icon = BEM.add_modifier root "with-trailing-icon"

  (** Styles the text field as a text field in focus. *)
  let focused = BEM.add_modifier root "focused"

  (** Styles the text field that has no label. *)
  let no_label = BEM.add_modifier root "no-label"

  (** Styles the container of helper text and character counter elements. *)
  let helper_line = root ^ "-helper-line"

  let icon = BEM.add_element root "icon"

  let input = BEM.add_element root "input"

  module Helper_text = struct
    (** Mandatory. *)
    let root = root ^ "-helper-text"

    (** Makes the helper text permanently visible. *)
    let persistent = BEM.add_modifier root "persistent"

    (** Indicates the helper text is a validation message. *)
    let validation_msg = BEM.add_modifier root "validation-msg"
  end

  module Character_counter = struct
    (** Mandatory. *)
    let root = root ^ "-character-counter"
  end
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  module Helper_text = struct
    let create ?(classes = []) ?attrs
          ?(persistent = false) ?(validation = false) ?text () : 'a elt =
      let classes =
        classes
        |> cons_if validation CSS.Helper_text.validation_msg
        |> cons_if persistent CSS.Helper_text.persistent
        |> List.cons CSS.Helper_text.root in
      let text = match text with
        | None -> ""
        | Some s -> s in
      div ~a:([a_class classes]
              |> cons_if (not persistent) @@ a_aria "hidden" ["true"]
              <@> attrs)
        [txt text]
  end

  module Character_counter = struct
    let create ?(classes = []) ?attrs ?(cur = 0) ~max () : 'a elt =
      let classes = CSS.Character_counter.root :: classes in
      let text = Printf.sprintf "%d / %d" cur max in
      div ~a:([a_class classes] <@> attrs) [txt text]
  end

  module Textarea = struct
    let create_textarea ?(classes = []) ?attrs ?id
          ?value ?placeholder ?(required = false)
          ?min_length ?max_length ?rows ?cols
          ?(disabled = false) () : 'a elt =
      let classes = CSS.input :: classes in
      let text = match value with
        | None -> ""
        | Some s -> s in
      textarea ~a:([a_class classes]
                   |> map_cons_option a_id id
                   |> cons_if_lazy disabled a_disabled
                   |> cons_if_lazy required a_required
                   |> map_cons_option a_placeholder placeholder
                   |> map_cons_option a_minlength min_length
                   |> map_cons_option a_maxlength max_length
                   |> map_cons_option a_rows rows
                   |> map_cons_option a_cols cols
                   <@> attrs)
        (txt text)

    let create ?(classes = []) ?attrs ?(disabled = false) ?(no_label = false)
          ?(fullwidth = false) ?(focused = false)
          ?character_counter ?line_ripple ?label ?outline ~input
          () : 'a elt =
      let no_label = match line_ripple, label with
        | Some _, None -> true
        | _ -> no_label in
      let outlined = match outline with
        | None -> false
        | Some _ -> true in
      let classes =
        classes
        |> cons_if no_label CSS.no_label
        |> cons_if fullwidth CSS.fullwidth
        |> cons_if disabled CSS.disabled
        |> cons_if outlined CSS.outlined
        |> cons_if focused CSS.focused
        |> List.cons CSS.textarea
        |> List.cons CSS.root in
      div ~a:([a_class classes] <@> attrs)
        (character_counter ^:: input ^:: label ^:: outline ^:: line_ripple ^:: [])
  end

  let create_input ?(classes = []) ?attrs ?id
        ?pattern ?min_length ?max_length ?step
        ?value ?placeholder ?(required = false)
        ?(disabled = false) ?(typ = `Text)
        () : 'a elt =
    let classes = CSS.input :: classes in
    input ~a:([ a_class classes
              ; a_input_type typ ]
              |> cons_if_lazy disabled a_disabled
              |> map_cons_option a_id id
              |> map_cons_option a_pattern pattern
              |> map_cons_option a_minlength min_length
              |> map_cons_option a_maxlength max_length
              |> map_cons_option a_step step
              |> cons_if_lazy required a_required
              |> map_cons_option a_placeholder placeholder
              |> map_cons_option a_value value
              <@> attrs) ()

  let create ?(classes = []) ?attrs ?(disabled = false)
        ?leading_icon ?trailing_icon ?(no_label = false)
        ?(fullwidth = false) ?(textarea = false) ?(focused = false)
        ?line_ripple ?label ?outline ~input () : 'a elt =
    let no_label = match line_ripple, label with
      | Some _, None -> true
      | _ -> no_label in
    let outlined = match outline with
      | None -> false
      | Some _ -> true in
    let with_leading_icon = match leading_icon with
      | None -> false
      | Some _ -> true in
    let with_trailing_icon = match trailing_icon with
      | None -> false
      | Some _ -> true in
    let classes =
      classes
      |> cons_if no_label CSS.no_label
      |> cons_if textarea CSS.textarea
      |> cons_if fullwidth CSS.fullwidth
      |> cons_if disabled CSS.disabled
      |> cons_if outlined CSS.outlined
      |> cons_if focused CSS.focused
      |> cons_if with_leading_icon CSS.with_leading_icon
      |> cons_if with_trailing_icon CSS.with_trailing_icon
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs)
      (leading_icon ^:: input ^:: label ^:: trailing_icon
       ^:: outline ^:: line_ripple ^:: [])
end
