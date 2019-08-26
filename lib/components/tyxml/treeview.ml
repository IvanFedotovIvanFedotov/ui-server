module CSS = struct
  let root = "mdc-treeview"

  let node = root ^ "-node"

  let node_content = BEM.add_element node "content"

  let node_children = BEM.add_element node "children"

  let node_expander = BEM.add_element node "expander"

  let node_expanded = BEM.add_modifier node "expanded"

  let node_selected = BEM.add_modifier node "selected"

  let node_activated = BEM.add_modifier node "activated"

end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap
     with module Xml := Xml
      and module Svg := Svg) = struct
  open Html
  open Utils

  module Item_list_ = Item_list.Make(Xml)(Svg)(Html)

  let create_children ?(classes = []) ?(attrs = []) nodes : 'a elt =
    let classes = CSS.node_children :: classes in
    ul ~a:([ a_class classes
           ; a_role ["group"]] @ attrs) nodes

  let create_node_expander ?(classes = []) ?(attrs = []) () =
    let classes = CSS.node_expander :: Item_list.CSS.item_meta :: classes in
    span ~a:([a_class classes] @ attrs) []

  let create_node_primary_text = Item_list_.create_item_primary_text

  let create_node_secondary_text = Item_list_.create_item_secondary_text

  let create_node_text = Item_list_.create_item_text

  let create_node_content ?(classes = []) ?attrs ?graphic ?meta ?role text : 'a elt =
    let classes = CSS.node_content :: classes in
    Item_list_.create_item' ~classes ?attrs ?graphic ?meta ?role ~text span

  let create_node' ?(classes = []) ?(attrs = [])
      ?value ?level ?children
      ?(expanded = false)
      ?(tabindex = (-1))
      ?(selected = false)
      ?(checked = false)
      ?(indeterminate = false)
      ~content () : 'a elt =
    let checked =
      if indeterminate
      then "mixed"
      else if checked
      then "true"
      else "false" in
    let classes = CSS.node :: classes in
    li ~a:([ a_class classes
           ; a_role ["treeitem"]
           ; a_aria "selected" [string_of_bool selected]
           ; a_aria "checked" [checked]
           ; a_aria "expanded" [string_of_bool expanded]
           ; a_tabindex tabindex ] @ attrs
           |> map_cons_option (fun x -> a_aria "level" [string_of_int x]) level
           |> map_cons_option (a_user_data "value") value)
      (match children with
       | None -> [content]
       | Some x -> [content; x])

  let create_node ?classes ?attrs ?value
      ?tabindex ?selected ?checked ?indeterminate ?expanded
      ?graphic ?meta ?children ?secondary_text text : 'a elt =
    let text = match secondary_text with
      | None -> create_node_text [txt text] ()
      | Some s ->
        let primary = create_node_primary_text text () in
        let secondary = create_node_secondary_text s () in
        create_node_text [primary; secondary] () in
    let children = match children with
      | None | Some [] -> None
      | Some x -> Some (create_children x) in
    let meta = match meta, children with
      | Some _ as x, _ -> x
      | None, None -> None
      | None, Some _ -> Some (create_node_expander ()) in
    let content = create_node_content ?graphic ?meta text in
    create_node' ?classes ?attrs ?value ?children
      ?tabindex ?selected ?checked ?indeterminate ?expanded
      ~content ()

  let create ?(classes = []) ?(attrs = [])
      ?multiselectable
      ?(dense = false)
      ?(two_line = false)
      nodes =
    let classes =
      classes
      |> cons_if dense Item_list.CSS.dense (* FIXME *)
      |> cons_if two_line Item_list.CSS.two_line (* FIXME *)
      |> List.cons CSS.root in
    ul ~a:([ a_class classes
           ; a_role ["tree"]] @ attrs
           |> map_cons_option (fun x ->
               let v = string_of_bool x in
               a_aria "multiselectable" [v])
             multiselectable)
      nodes

end
