module CSS = struct
  (** Required on the root element. *)
  let root = "mdc-menu"

  (** Used to wrap a group of mdc-list-item elements that will represent
      a selection group. *)
  let selection_group = BEM.add_element root "selection-group"

  (** Required when using a selection group to indicate which item is
      selected. Should contain an icon or svg that indicates the selected
      state of the list item. *)
  let selection_group_icon = BEM.add_element root "selection-group-icon"

  (** Used to indicate which element in a selection group is selected. *)
  let item_selected = BEM.add_modifier "mdc-menu-item" "selected"
end
