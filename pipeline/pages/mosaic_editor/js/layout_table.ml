open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

module Selector = struct
  let item = ".resizable" (* FIXME *)
end

module Divider = struct

  class t elt () =
    object
      inherit Widget.t elt ()
    end

  let make () : t =
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_divider () in
    new t elt ()

end

module Item = struct

  class t elt () =
    object
      inherit Widget.t elt ()
    end

  let make () : t =
    let elt = Tyxml_js.To_dom.of_element
      @@ Markup.create_table_item () in
    new t elt ()

end

class t ~width ~height ?(items = []) elt () =
  object(self)
    inherit Widget.t elt () as super
    val aspect = float_of_int width /. float_of_int height
    val width = width
    val height = height

    method! init () : unit =
      super#init ();
      List.iter super#append_child items;
      super#add_class Markup.CSS.table

    method! initial_sync_with_dom () : unit =
      let _ = Ui_templates.Resize_observer.observe
          ~f:(fun _ -> self#fit ())
          ~node:super#root
          () in
      Lwt.async (fun () ->
          Events.listen_lwt super#root Resizable.Event.input (fun e _ ->
              let target = Dom_html.eventTarget e in
              let position =
                Resizable.Position.of_client_rect
                @@ Widget.event_detail e in
              let aspect_ratio =
                match Element.get_attribute target Position.Attr.keep_aspect_ratio with
                | Some "true" -> Some (Position.get_aspect_ratio target)
                | _ -> None in
              let adjusted, lines =
                Resizable.Sig.adjust_position
                  ?aspect_ratio
                  target
                  position
                  self#items
                  (super#root##.offsetWidth, super#root##.offsetHeight) in
              Resizable.Position.apply_to_element adjusted target;
              Lwt.return_unit));
      super#initial_sync_with_dom ()

    method! layout () : unit =
      List.iter Widget.layout items;
      self#fit ();
      super#layout ()

    method fit () : unit =
      let scale_factor = self#scale_factor in
      let width' = int_of_float @@ float_of_int width *. scale_factor in
      let height' = int_of_float @@ float_of_int height *. scale_factor in
      super#root##.style##.width := Utils.px_js width';
      super#root##.style##.height := Utils.px_js height';
      List.iter (fun item ->
          let w = float_of_int @@ Position.get_width item in
          let h = float_of_int @@ Position.get_height item in
          let left = float_of_int @@ Position.get_left item in
          let top = Position.get_top item in
          let new_w, new_h =
            let w' = w *. scale_factor in
            w', Position.get_height_for_width item w' in
          let new_left = (left *. new_w) /. w in
          let new_top = (top * new_h) / int_of_float h in
          item##.style##.top := Utils.px_js new_top;
          item##.style##.left := Utils.px_js @@ Float.to_int @@ Float.floor new_left;
          item##.style##.width := Utils.px_js @@ Float.to_int @@ Float.floor new_w;
          item##.style##.height := Utils.px_js new_h)
        self#items

    method items : Dom_html.element Js.t list =
      Element.query_selector_all super#root Selector.item

    method private parent_rect : float * float * float =
      Js.Opt.case (Element.get_parent super#root)
        (fun () -> 0., 0., 1.)
        (fun x ->
           let width = float_of_int x##.offsetWidth in
           let height = float_of_int x##.offsetHeight in
           width, height, width /. height)

    method private scale_factor : float =
      let cur_width, cur_height, cur_aspect = self#parent_rect in
      if cur_aspect > aspect
      then cur_height /. float_of_int height
      else cur_width /. float_of_int width
  end

let make_item x y w h =
  let item = Resizable.make () in
  let ar = (float_of_int w) /. (float_of_int h) in
  item#set_attribute Position.Attr.width (string_of_int w);
  item#set_attribute Position.Attr.height (string_of_int h);
  item#set_attribute Position.Attr.left (string_of_int x);
  item#set_attribute Position.Attr.top (string_of_int y);
  item#set_attribute Position.Attr.aspect_ratio (Printf.sprintf "%g" ar);
  item#root##.style##.width := Utils.px_js w;
  item#root##.style##.height := Utils.px_js h;
  item#root##.style##.top := Utils.px_js y;
  item#root##.style##.left := Utils.px_js x;
  item

let make () =
  let items =
    [ make_item 0 0 111 150
    ; make_item 111 0 189 150
    ; make_item 0 150 200 150
    ; make_item 210 150 90 150
    ] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Tyxml_js.Html.(div []) in
  new t ~width:1920 ~height:1080 ~items elt ()