open Js_of_ocaml
open Utils

include Components_tyxml.Item_list
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type selection =
  | Single
  | Multiple

module Attr = struct
  let aria_checked = "aria-checked"
  let aria_current = "aria-current"
  let aria_orientation = "aria-orientation"
  let aria_selected = "aria-selected"
end

module Selector = struct
  let enabled_items = Printf.sprintf ".%s:not(.%s)" CSS.item CSS.item_disabled
  let aria_checked_checkbox = "[role=\"checkbox\"][aria-checked=\"true\"]"
  let aria_checked_radio = "[role=\"radio\"][aria-checked=\"true\"]"
  let radio = "input[type=\"radio\"]:not(:disabled)"
  let checkbox = "input[type=\"checkbox\"]:not(:disabled)"
  let checkbox_radio = Printf.sprintf "%s, %s" checkbox radio
end

module Item = struct

  class t ?(ripple = false) (elt : Dom_html.element Js.t) () =
  object(self)
    val _text : Dom_html.element Js.t =
      find_element_by_class_exn elt CSS.item_text
    val _ripple : Ripple.t option =
      if not ripple then None else Some (Ripple.attach elt)
    inherit Widget.t elt () as super

    method! layout () : unit =
      super#layout ();
      Option.iter Ripple.layout _ripple

    method secondary_text : string option =
      match Element.query_selector _text ("." ^ CSS.item_secondary_text) with
      | None -> None
      | Some elt -> Js.Opt.to_option @@ Js.Opt.map elt##.textContent Js.to_string

    method set_secondary_text (s : string) : unit =
      match Element.query_selector _text ("." ^ CSS.item_secondary_text) with
      | Some elt -> elt##.textContent := Js.some (Js.string s)
      | None ->
         (match Element.query_selector _text ("." ^ CSS.item_primary_text) with
          | Some _ -> ()
          | None ->
             let text = match self#text with
               | None -> ""
               | Some s -> s in
             let x =
               Tyxml_js.To_dom.of_element
               @@ Markup.create_item_primary_text text () in
             Element.insert_child_at_index ~child:x 0 _text);
         let secondary =
           Tyxml_js.To_dom.of_element
           @@ Markup.create_item_secondary_text s () in
         Element.insert_child_at_index ~child:secondary 1 _text

    method text : string option =
      match Element.query_selector _text ("." ^ CSS.item_primary_text) with
      | None -> Js.Opt.to_option @@ Js.Opt.map _text##.textContent Js.to_string
      | Some elt -> Js.Opt.to_option @@ Js.Opt.map elt##.textContent Js.to_string

    method set_text (s : string) : unit =
      match Element.query_selector _text ("." ^ CSS.item_primary_text) with
      | None -> _text##.textContent := Js.some (Js.string s)
      | Some elt -> elt##.textContent := Js.some (Js.string s)

    method activated : bool =
      self#has_class CSS.item_activated

    method selected : bool =
      self#has_class CSS.item_selected

    method ripple : Ripple.t option =
      _ripple

  end

  let make ?ripple ?activated ?selected ?secondary_text ?graphic ?meta ?role
        ~tag text : t =
    let text_elt = match secondary_text with
      | Some st ->
         let primary = Markup.create_item_primary_text text () in
         let secondary = Markup.create_item_secondary_text st () in
         Markup.create_item_text [primary; secondary] ()
      | None -> Markup.create_item_text [Tyxml_js.Html.txt text] () in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_item
           ?graphic:(Option.map Widget.to_markup graphic)
           ?meta:(Option.map Widget.to_markup meta)
           ?activated ?selected ?role
           ~tag
           text_elt () in
    new t ?ripple elt ()

  let attach ?ripple (elt : #Dom_html.element Js.t) : t =
    new t ?ripple (Element.coerce elt) ()

end

class t (elt : Dom_html.element Js.t) () =
object(self)
  val mutable _selected_indexes : int list = []
  val mutable _is_checkbox_list = false
  val mutable _is_radio_list = false
  val mutable _is_single_selection = false
  inherit Widget.t elt () as super

  method! layout () : unit =
    super#layout ();
    match List.length self#list_elements with
    | 0 -> ()
    | _ ->
       if self#has_checkbox_at_index 0
       then _is_checkbox_list <- true
       else if self#has_radio_at_index 0
       then _is_radio_list <- true

  method dense : bool =
    super#has_class CSS.dense

  method set_dense (x : bool) : unit =
    super#toggle_class ~force:x CSS.dense

  method non_interactive : bool =
    super#has_class CSS.non_interactive

  method set_non_interactive (x : bool) : unit =
    super#toggle_class ~force:x CSS.non_interactive

  (* Private methods *)

  method private handle_click () : unit =
    ()

  method private is_selectable_list : bool =
    _is_single_selection || _is_checkbox_list || _is_radio_list

  method private set_tabindex_to_first_selected_item () : unit =
    if self#is_selectable_list
    then (
    )

  method private is_index_valid (i : int) : bool =
    false

  method private list_elements : Dom_html.element Js.t list =
    Element.query_selector_all super#root Selector.enabled_items

  method private has_radio_at_index (i : int) : bool =
    match List.nth_opt self#list_elements i with
    | None -> false
    | Some item -> Option.is_some @@ Element.query_selector item Selector.radio

  method private has_checkbox_at_index (i : int) : bool =
    match List.nth_opt self#list_elements i with
    | None -> false
    | Some item -> Option.is_some @@ Element.query_selector item Selector.checkbox

  method private is_item_checkbox_checked (item : Dom_html.element Js.t) : bool =
    match Element.query_selector item Selector.checkbox with
    | None -> false
    | Some x ->
       let (checkbox : Dom_html.inputElement Js.t) = Js.Unsafe.coerce x in
       Js.to_bool checkbox##.checked

  method private set_item_checked (item : Dom_html.element Js.t) (x : bool) : unit =
    match Element.query_selector item Selector.checkbox_radio with
    | None -> ()
    | Some elt ->
       let (input : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
       input##.checked := Js.bool x;
       let event =
         (Js.Unsafe.coerce Dom_html.document)##createEvent
           (Js.string "Event") in
       ignore @@ event##initEvent (Js.string "change") Js._true Js._true;
       (Js.Unsafe.coerce input)##dispatchEvent event

  method private toggle_checkbox_at_index ?(toggle = true) (i : int) : unit =
    match List.nth_opt self#list_elements i with
    | None -> ()
    | Some item ->
       let checked = not @@ self#is_item_checkbox_checked item in
       if toggle then self#set_item_checked item checked;
       if checked
       then _selected_indexes <- i :: _selected_indexes
       else _selected_indexes <- List.remove ~eq:(=) i _selected_indexes
end

  (* let make () : t =
   *   let two_line = match two_line with
   *     | Some x -> x
   *     | None ->
   *        List.find_pred (function
   *            | `Divider _ -> false
   *            | `Item x -> Option.is_some x#secondary_text)
   *          items
   *        |> Option.is_some in
   *   let (elt : Dom_html.element Js.t) =
   *     Markup.create ?avatar ~two_line
   *       ~items:(List.map (function
   *                   | `Divider x -> Widget.to_markup x
   *                   | `Item x -> Widget.to_markup x)
   *                 items) ()
   *     |> To_dom.of_element in
   *   new t elt () *)

  (* module List_group = struct
   * 
   *   type group =
   *     { subheader : Typography.Text.t option
   *     ; list : base
   *     }
   * 
   *   let rec add_dividers acc l =
   *     match l with
   *     | [] -> acc
   *     | hd :: [] -> List.rev @@ hd :: acc
   *     | hd :: tl ->
   *        add_dividers ((hd @ [Widget.to_markup @@ new Divider.t ()])
   *                      :: acc) tl
   * 
   *   class t ?(dividers=true) ~(content:group list) () =
   * 
   *     let elt =
   *       Markup.List_group.create
   *         ~content:(
   *           List.map (fun x ->
   *               let h = Option.map (fun w ->
   *                           w#add_class Markup.List_group.subheader_class;
   *                           Widget.to_markup w) x.subheader in
   *               [Widget.to_markup x.list]
   *               |> List.cons_maybe h)
   *             content
   *           |> (fun x -> if dividers then add_dividers [] x else x)
   *           |> List.flatten)
   *         ()
   *       |> Tyxml_js.To_dom.of_div in
   * 
   *     object
   *       inherit Widget.t elt ()
   * 
   *       method content = content
   *     end
   * 
   * end *)

