open Components

let base_class = "wm-left-toolbar"
let action_class = Components_tyxml.BEM.add_element base_class "action"

let make_action ?on_click (action : Wm_types.action) =
  let w = Fab.make ?on_click ~mini:true ~icon:action.icon () in
  w#add_class action_class;
  w#set_attribute "title" action.name;
  w

let make widgets =
  let box = Box.make ~dir:`Column widgets in
  box#add_class base_class;
  box
