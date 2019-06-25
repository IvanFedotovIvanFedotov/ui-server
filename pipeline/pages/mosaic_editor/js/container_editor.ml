open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

module Selector = struct
  let table_ghost = Printf.sprintf ".%s" Markup.CSS.grid_ghost (* FIXME *)
  let table_overlay = Printf.sprintf ".%s" Markup.CSS.grid_overlay (* FIXME *)
end

module Grid = struct

  type event = Touch of Dom_html.touchEvent Js.t
             | Mouse of Dom_html.mouseEvent Js.t

  (* TODO implement *)
  class t (elt : Dom_html.element Js.t) = object(self)
    inherit Widget.t elt () as super

    method! destroy () : unit =
      super#destroy ()
  end

end

class t ?(containers = []) ~resolution elt () = object(self)
  val grid_overlay = match Element.query_selector elt Selector.table_overlay with
    | None -> failwith "container-editor: table overlay element not found"
    | Some x ->
      let show_grid_lines = Storage.(get_bool ~default:true show_grid_lines) in
      Grid_overlay.attach ~show_grid_lines ~size:10 x
  val ghost = match Element.query_selector elt Selector.table_ghost with
    | None -> failwith "container-editor: table ghost element not found"
    | Some x -> x
  val grid =
    let make_cell ?(classes = []) ?(content = []) ?text () =
      Tyxml_js.Html.(
        let text = match text with
          | None -> []
          | Some x -> [span [txt x]] in
        div ~a:[a_class (["container-grid__cell"] @ classes)]
          ([ div ~a:[a_class ["container-grid__row-handle"]] []
           ; div ~a:[a_class ["container-grid__col-handle"]] []
           ; div ~a:[a_class ["container-grid__mul-handle"]] []
           ] @ text @ content)) in
    let nested_grid = Tyxml_js.Html.(
        div ~a:[a_class ["container-grid"]]
          [ make_cell ()
          ; make_cell ()
          ; make_cell ()
          ; make_cell ()
          ]) in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Tyxml_js.Html.(
          div ~a:[a_class ["container-grid"]]
            [ make_cell ~content:[nested_grid] ()
            ; make_cell ()
            ; make_cell ()
            ; make_cell ()
            ]) in
    let _ = Resizable_grid.attach (Tyxml_js.To_dom.of_element nested_grid) in
    let _ = Resizable_grid.attach elt in
    elt

  val mutable _containers : (string * Wm.container) list = containers
  val mutable _listeners = []
  val mutable _focused_item = None
  val mutable _toolbar = Widget.create_div ()

  inherit Drop_target.t elt () as super

  method! init () : unit =
    Element.append_child super#root grid;
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ keydowns super#root self#handle_keydown
        ]);
    super#initial_sync_with_dom ()

  method! layout () : unit =
    let scale_factor = self#scale_factor in
    let width' = int_of_float @@ float_of_int (fst resolution) *. scale_factor in
    let height' = int_of_float @@ float_of_int (snd resolution) *. scale_factor in
    super#root##.style##.width := Utils.px_js width';
    super#root##.style##.height := Utils.px_js height';
    grid_overlay#layout ();
    super#layout ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method toolbar = _toolbar

  (* Private methods *)

  method private handle_keydown _ _ : unit Lwt.t =
    (* TODO implement *)
    Lwt.return_unit

  method private parent_rect : float * float * float =
    Js.Opt.case (Element.get_parent super#root)
      (fun () -> 0., 0., 1.)
      (fun x ->
         let width = float_of_int x##.offsetWidth in
         let height = float_of_int x##.offsetHeight in
         width, height, width /. height)

  method private scale_factor : float =
    let cur_width, cur_height, cur_aspect = self#parent_rect in
    if cur_aspect > (float_of_int (fst resolution) /. float_of_int (snd resolution))
    then cur_height /. float_of_int (snd resolution)
    else cur_width /. float_of_int (fst resolution)

  method private handle_dropped_json (json : Yojson.Safe.json) : unit Lwt.t =
    Lwt.return_unit

  method private move_ghost ?aspect event : unit =
    ()
end

let make (wm : Wm.t) =
  let content =
    Markup.create_grid_overlay ()
    :: Markup.create_grid_ghost ()
    :: [] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Tyxml_js.Html.(div ~a:[a_class ["container-editor"]]
                        content) in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    elt ()
