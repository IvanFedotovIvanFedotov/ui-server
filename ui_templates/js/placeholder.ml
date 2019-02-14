open Js_of_ocaml
open Containers
open Components

module CSS = struct
  let root = "mdc-placeholder"
  let content = CSS.add_element root "content"
  let widget = CSS.add_element root "widget"
  let text = CSS.add_element root "text"

  let icon = CSS.add_modifier root "icon"
  let progress = CSS.add_modifier root "progress"
  let error = CSS.add_modifier root "error"
end

let error_svg_path = Icon.SVG.Path.alert_decagram

module Base = struct

  class t ~widget ~text () =
    let box =
      new Vbox.t
        ~halign:`Center
        ~widgets:[widget#widget; text#widget] () in
    object
      inherit Widget.t Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#init ();
        widget#add_class CSS.widget;
        text#add_class CSS.text;
        box#add_class CSS.content;
        super#add_class CSS.root;
        super#append_child box
    end
end

module With_icon = struct

  class ['a] t ?action ~text ~(icon  : (#Widget.t) as 'a) () =
    let ico = match action with
      | Some f ->
         let btn = Icon_button.make ~icon () in
         btn#listen_click_lwt' (fun e _ -> f e; Lwt.return_unit);
         btn#widget
      | None -> icon#widget in
    let text =
      Typography.Text.make
        ~split:true
        ~adjust_margin:false
        text
        () in
    object
      inherit Base.t ~widget:ico ~text () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.icon

      method text_widget = text
      method icon : 'a = icon
      method set_text (s : string) : unit =
        text#set_text s
    end

  let make ?action ~text ~icon () =
    new t ?action ~text ~icon ()

end

module Progress = struct

  let make_dot () =
    let dot = Widget.create_span () in
    dot#set_text_content ".";
    dot

  let create_text text =
    let w = Widget.create @@ Dom_html.(createP document) in
    w#set_text_content text;
    List.iter (fun _ -> w#append_child @@ make_dot ())
    @@ List.range' 0 3;
    w

  class t ?(indeterminate = true) ?size ?(text = "Загрузка") () =
    let w = Circular_progress.make ?size ~indeterminate () in
    let p = create_text text in
    object
      inherit Base.t ~widget:w ~text:p () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.progress

      method progress = w
    end

  let make ?indeterminate ?size ?text () =
    new t ?indeterminate ?size ?text ()

end

module Err = struct

  let create_error_icon () =
    Icon.SVG.(make_simple error_svg_path)

  let make ?action ?icon ?(text = "error") () =
    let icon = match icon with
      | Some icon -> icon
      | None -> create_error_icon () in
    let ph = With_icon.make ?action ~icon ~text () in
    ph#add_class CSS.error;
    ph

end

let make_under_development () =
  let icon = Icon.SVG.(make_simple Path.crane) in
  let text = Typography.Text.make "Страница находится в разработке" () in
  let ph = new Base.t ~widget:icon ~text () in
  ph#add_class CSS.icon;
  ph
