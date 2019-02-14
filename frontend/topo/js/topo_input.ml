let input_to_string : Common.Topology.input -> string = function
  | RF -> "RF"
  | ASI -> "ASI"
  | TSOIP -> "TSoIP"

let circle_markup text =
  Tyxml_js.Html.(
    span ~a:[a_class ["topology__input__circle"]] [txt text])

let label_markup text =
  Tyxml_js.Html.(
    span ~a:[a_class ["topology__input__label"]] [txt text])

let make_icon ?classes path =
  let open Components.Icon.Markup.SVG in
  let path = create_path path () in
  let icon = create ?classes [path] () in
  icon

let markup (input : Common.Topology.topo_input) =
  (* FIXME migrate to svg icon *)
  let name = Common.Topology.get_input_name input in
  Tyxml_js.Html.(
    div ~a:[a_class ["mdc-chip"; "topology__input"]]
      [ div ~a:[a_class ["mdc-chip__text"]] [txt name]
      ; make_icon
          ~classes:["mdc-chip__icon mdc-chip__icon--trailing"]
          Components.Icon.SVG.Path.arrow_right
      ])

class t ~input () =
  let body = Tyxml_js.To_dom.of_element @@ markup input in
  object
    method input = input
    inherit Topo_node.t ~node:(`Entry (Input input)) ~body body ()
  end

let create input = new t ~input ()
