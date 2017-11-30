module Base = struct

  class type mdc =
    object
      method value           : Js.js_string Js.t Js.readonly_prop
      method options         : Dom_html.element Js.js_array Js.t Js.readonly_prop
      method selectedIndex   : int Js.prop
      method selectedOptions : Dom_html.element Js.js_array Js.t Js.readonly_prop
      method disabled        : bool Js.t Js.prop
      method item            : (Js.number Js.t -> Dom_html.element Js.t Js.opt) Js.meth
      method nameditem       : (Js.js_string Js.t -> Dom_html.element Js.t Js.opt) Js.meth
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail_ : mdc Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCSelect:change"
    }

  module Item = struct
    class t ?id ?start_detail ?end_detail ~text () = object
      inherit Menu.Item.t ?start_detail ?end_detail ~text () as super
      initializer
        CCOpt.iter (fun x -> super#set_id x) id;
        super#set_attribute "role" "option"
    end
  end

  class t ?placeholder ~(items:Item.t list) () =

    let menu = new Menu.t ~items:(List.map (fun x -> `Item (x : Item.t :> Menu.Item.t)) items) () in
    let () = menu#add_class Markup.Select.Base.menu_class in
    let elt = Markup.Select.Base.create ?selected_text:placeholder ~menu:(Widget.widget_to_markup menu) ()
              |> Tyxml_js.To_dom.of_element in

    object(self)

      inherit Widget.widget elt ()

      val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.select##.MDCSelect##attachTo x)
      val items = items

      method get_menu    = menu
      method set_dense x = menu#get_list#set_dense x
      method get_value   = Js.to_string mdc##.value (* id of item (if available) or text *)

      method get_items   = items
      method get_length  = CCList.length self#get_items
      method get_item n  = CCList.get_at_idx n self#get_items
      method get_named_item key = CCList.find_pred (fun x -> x#get_id = key && (match x#get_attribute "name" with
                                                                                | Some n -> n = key
                                                                                | None   -> false))
                                                   self#get_items

      method get_selected_index = mdc##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method get_selected_item  = CCOpt.map (fun x -> CCList.get_at_idx x self#get_items) self#get_selected_index
      method select_at_index i  = mdc##.selectedIndex := i
      method select_item i      = (match CCList.find_idx (fun x -> x == i) self#get_items with
                                   | Some (idx,_) -> self#select_at_index idx
                                   | None         -> ())

      method get_disabled   = Js.to_bool mdc##.disabled
      method set_disabled x = mdc##.disabled := Js.bool x

    end

end

module Pure = struct

  module Item = struct

    class t ?disabled ?value ~text () =

      let elt = Markup.Select.Pure.Item.create ?disabled ?value ~text () |> Tyxml_js.To_dom.of_option in

      object(self)

        inherit Widget.widget elt ()

        method option_element = elt

        method get_text    = Js.to_string self#option_element##.text
        method get_value   = Js.to_string self#option_element##.value
        method set_value v = self#option_element##.value := Js.string v
        method get_index   = self#option_element##.index

        method is_selected    = Js.to_bool self#option_element##.selected
        method set_selected x = self#option_element##.selected := Js.bool x

        method get_disabled   = Js.to_bool self#option_element##.disabled
        method set_disabled x = self#option_element##.disabled := Js.bool x

      end

  end

  module Group = struct

    class t ~label ~(items:Item.t list) () =

      let item_elts = List.map (fun x -> Tyxml_js.Of_dom.of_option x#option_element) items in
      let elt = Markup.Select.Pure.Item.create_group ~label ~items:item_elts () |> Tyxml_js.To_dom.of_optgroup in

      object(self)

        inherit Widget.widget elt ()

        method opt_group_element = elt

        method get_items   = items
        method get_label   = Js.to_string self#opt_group_element##.label
        method set_label s = self#opt_group_element##.label := Js.string s

        method get_disabled   = Js.to_bool self#opt_group_element##.disabled
        method set_disabled x = self#opt_group_element##.disabled := Js.bool x

      end

  end

  class t ~(items : [ `Item of Item.t | `Group of Group.t ] list) () =

    let item_elts = List.map (function
                              | `Group g -> Widget.widget_to_markup g
                              | `Item i  -> Widget.widget_to_markup i) items in
    let elt = Markup.Select.Pure.create ~items:item_elts () |> Tyxml_js.To_dom.of_select in

    object(self)

      inherit Widget.widget elt ()

      method select_element = elt

      method get_value = Js.to_string self#select_element##.value
      method get_items = CCList.fold_left (fun acc x -> match x with
                                                        | `Group g -> acc @ g#get_items
                                                        | `Item i  -> acc @ [i]) [] items

      method get_length = self#select_element##.length
      method get_item n = CCList.get_at_idx n self#get_items

      method get_selected_index = self#select_element##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method get_selected_item  = CCOpt.map (fun x -> CCList.get_at_idx x self#get_items) self#get_selected_index
      method select_at_index i  = self#select_element##.selectedIndex := i

      method get_disabled   = Js.to_bool self#select_element##.disabled
      method set_disabled x = self#select_element##.disabled := Js.bool x

    end

end

module Multi = struct

  module Divider = struct
    class t () = object
      inherit Widget.widget (Markup.Select.Multi.Item.create_divider () |> Tyxml_js.To_dom.of_option) ()
    end
  end

  module Item = struct
    class t ?disabled ?value ~text () = object
      inherit Pure.Item.t ?disabled ?value ~text () as super
      initializer
        super#add_class Markup.List_.Item._class
    end
  end

  module Group = struct
    class t ~label ~(items:Item.t list) () = object
      inherit Pure.Group.t ~label ~items:(List.map (fun x -> (x : Item.t :> Pure.Item.t)) items) () as super
      initializer
        super#add_class Markup.List_.List_group._class
    end
  end

  class t ?size ~(items:[ `Item of Item.t | `Divider of Divider.t | `Group of Group.t ] list) () =

    let item_elts = List.map (function
                              | `Divider d -> Widget.widget_to_markup d
                              | `Group g   -> Widget.widget_to_markup g
                              | `Item i    -> i#style##.paddingLeft := Js.string "32px"; Widget.widget_to_markup i)
                             items in
    let elt = Markup.Select.Multi.create ?size ~items:item_elts () |> Tyxml_js.To_dom.of_select in

    object(self)

      inherit Widget.widget elt ()

      method select_element = elt

      method get_items = CCList.fold_left (fun acc x -> match x with
                                                        | `Divider _ -> acc
                                                        | `Group g   -> acc @ g#get_items
                                                        | `Item i    -> acc @ [i]) [] items

      method get_length = self#select_element##.length
      method get_item n = CCList.get_at_idx n self#get_items

      method get_selected_index = self#select_element##.selectedIndex |> (fun x -> if x = -1 then None else Some x)
      method get_selected_item  = CCOpt.map (fun x -> CCList.get_at_idx x self#get_items) self#get_selected_index
      method select_at_index i  = self#select_element##.selectedIndex := i

      method get_disabled   = Js.to_bool self#select_element##.disabled
      method set_disabled x = self#select_element##.disabled := Js.bool x

    end

end
