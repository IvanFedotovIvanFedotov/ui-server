open Containers
open Dashboard_common

class t ~(title : string) ~widgets () =
  let title = new Typography.Text.t ~adjust_margin:false ~text:title () in
  let content = title#widget :: List.map Widget.coerce widgets in
  object
    inherit Side_sheet.t  (`Content content) () as super

    method! init () : unit =
      super#init ();
      title#add_class Markup.Panel.title_class;
      super#add_class Markup.Panel._class
  end

class add ~(widgets : Dashboard_add_item.t list) () =
  let e =
    List.map (fun x -> x#s_dragging) widgets
    |> React.S.merge ~eq:Equal.bool (||) false
    |> React.S.Bool.rise in
  object
    val mutable _e = None
    inherit t ~title:"Добавить виджет" ~widgets () as super

    method! init () : unit =
      super#init ();
      (* timeout needed to prevent d&d cancellation *)
      _e <- Some (React.E.map (fun () -> Utils.set_timeout super#hide 0.) e)

    method! destroy () : unit =
      super#destroy ();
      Option.iter (React.E.stop ~strong:true) _e;
      _e <- None;
  end
