open Containers
open Dashboard_common
open Dashboard_item

let drag_type = "application/dashboard-item"

let _class               = Markup.CSS.add_element base_class "add-item"
let thumbnail_class      = Markup.CSS.add_element _class "thumbnail"
let thumbnail_icon_class = Markup.CSS.add_modifier thumbnail_class "icon"
let text_box_class       = Markup.CSS.add_element _class "text-box"
let title_class          = Markup.CSS.add_element _class "title"
let description_class    = Markup.CSS.add_element _class "description"
let dragging_class       = Markup.CSS.add_modifier _class "dragging"

class t (info:info) () =
  let adjust_margin = false in
  let data          = info_to_yojson info |> Yojson.Safe.to_string |> Js.string in
  let typ           = drag_type in
  let title         = new Typography.Text.t ~adjust_margin ~text:info.title () in
  let description   = new Typography.Text.t ~adjust_margin ~text:info.description () in
  let text_box      = new Box.t ~vertical:true ~widgets:[title#widget;description#widget] () in
  let thumbnail     = match info.thumbnail with
    | `Icon icon -> new Icon.Font.t ~icon ()
                    |> fun x -> x#add_class thumbnail_icon_class; x#widget
  in
  let box    = new Box.t ~vertical:false ~widgets:[thumbnail;text_box#widget] () in
  let s,push = React.S.create false in
  object(self)
    inherit Widget.widget box#root ()
    inherit Touch_draggable.t ~data ~typ box#root ()

    method s_dragging = s

    initializer

      Dom_events.(listen self#root Typ.dragstart (fun _ e ->
                           push true;
                           self#add_class dragging_class;
                           e##.dataTransfer##setData (Js.string typ) data;
                           true)) |> ignore;
      Dom_events.(listen self#root Typ.dragend (fun _ e ->
                           push false;
                           let _ = e##.dataTransfer##.dropEffect |> Js.to_string in
                           self#remove_class dragging_class;
                           false)) |> ignore;

      self#set_attribute "draggable" "true";
      title#add_class title_class;
      description#add_class description_class;
      text_box#add_class text_box_class;
      thumbnail#add_class thumbnail_class;
      self#add_class _class
  end