open Js_of_ocaml
open Js_of_ocaml_tyxml
open Pipeline_types
open Components

type event =
  [ `Container of Wm.container
  ]

let ( >>= ) = Lwt.bind

let widget_of_yojson =
  Util_json.(Pair.of_yojson String.of_yojson Wm.widget_of_yojson)

include Page_mosaic_editor_tyxml.Widget_editor
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Selector = struct
  let item = Printf.sprintf ".%s" CSS.grid_item
  let grid_overlay = Printf.sprintf ".%s" CSS.grid_overlay
  let grid_ghost = Printf.sprintf ".%s" CSS.grid_ghost
end

let widget_type_to_string : Wm.widget_type -> string = function
  | Video -> "video"
  | Audio -> "audio"

let compare_pair o_x o_y (x1, y1) (x2, y2) =
  let c = o_x x1 x2 in
  if c = 0
  then o_y y1 y2
  else c

let set_tab_index ?prev
    (items : Dom_html.element Js.t list Lazy.t)
    (item : Dom_html.element Js.t) : unit =
  let set (i : int) (elt : Dom_html.element Js.t) =
    Element.set_attribute elt "tabindex" (string_of_int i) in
  (match prev with
   | Some prev -> if not @@ Element.equal item prev then set (-1) prev
   | None ->
     (* If no list item was selected, set first list item's tabindex to -1.
        Generally, tabindex is set to 0 on first list item of list that has
        no preselected items *)
     match Lazy.force items with
     | first :: _ -> if not @@ Element.equal first item then (set (-1) first)
     | _ -> ());
  set 0 item

let make_item_icon (widget : Wm.widget) =
  let path = match widget.type_ with
    | Video -> Icon.SVG.Path.video
    | Audio -> Icon.SVG.Path.music in
  Icon.SVG.make_simple path

let make_item_content (widget : Wm.widget) =
  let pid = match widget.pid with
    | None -> None
    | Some pid ->
      let text = Printf.sprintf "PID: %d" pid in
      Some (Typography.Text.make text)#markup in
  let ( ^:: ) x l = match x with None -> l | Some x -> x :: l in
  let text = Typography.Text.make widget.description in
  let icon = make_item_icon widget in
  Tyxml_js.To_dom.of_element
  @@ Tyxml_js.Html.(
      div ~a:[a_class [CSS.grid_item_content]]
        (icon#markup :: (pid ^:: [text#markup])))

let make_item ?parent_aspect ~parent_position (id, widget : string * Wm.widget) =
  let item = Resizable.make ~classes:[CSS.grid_item] () in
  item#root##.id := Js.string id;
  Widget_utils.set_attributes ~parent_position item#root widget;
  Element.append_child item#root (make_item_content widget);
  item



module Tmp_bring_to = struct
      
    (* need functions:*)
    let get_z_of_item (elt : Dom_html.element Js.t) : int =        
       1

    let set_z_of_item (elt : Dom_html.element Js.t) (z : int) : int =
       1
    
    let get_all_items : Dom_html.element Js.t list =
       []
              

    let rec set_all_z
       (zib_all_items : (int * (Dom_html.element Js.t) * bool ) list) =
     match zib_all_items with
       | [] -> []
       | hd :: tl -> let (z, i, _) = hd in
         let _ = set_z_of_item i z in 
         set_all_z zib_all_items

    let rec get_z_list
        (acc : int list)
        (items : Dom_html.element Js.t list) =
      match items with
        | [] -> acc
        | hd :: tl -> let acc = (get_z_of_item hd) :: acc in
          get_z_list acc tl
    
    let rec is_z_in_list 
        (is_in : bool) (* init false *)
        (z : int)
        (z_list : int list) =
      match z_list with
        | [] -> is_in
        | hd :: tl -> let is_in2 = if z = hd 
          then true 
          else false in
          is_z_in_list (if is_in = true || is_in2 = true then true else false) z tl

    let rec create_all_z_list    
        (acc : (int * (Dom_html.element Js.t) * bool ) list)  (* (z *
                                                              one of all_items *
                                                              true if it's selected item)
                                                            *)
        (all_items : Dom_html.element Js.t list)
        (selected_items_z : int list) =
      match all_items with
        | [] -> acc
        | hd :: tl -> let z = get_z_of_item hd in
          let acc = (z, hd, is_z_in_list false z selected_items_z ) :: acc in
          create_all_z_list acc tl selected_items_z
 
    let rec pack_list
        (counter : int) (* initial 1*)
        (acc : (int * (Dom_html.element Js.t) * bool ) list)  (* (z *
                                                               one of all_items *
                                                               true if it's selected item)
                                                               *)
        (zib_items  : (int * (Dom_html.element Js.t) * bool ) list) =
      match zib_items with
        | [] -> acc
        | hd :: tl -> let (_, i, b) = hd in
          let acc = (counter, i, b) :: acc in
          pack_list (counter + 1) acc tl 
    
    let rec get_upper_selected_z
        (counter : int) (* initial 1 *)
        (selected_list_len : int)
        (zib_items  : (int * (Dom_html.element Js.t) * bool ) list)
        : int =
      match zib_items with
        | [] -> ( -1)
        | hd :: tl -> let (_, _, b) = hd in
          if counter = selected_list_len 
          then counter
          else get_upper_selected_z 
            (if b then (counter + 1) else counter)
            selected_list_len tl 

    let rec get_first_selected_z
        (zib_items  : (int * (Dom_html.element Js.t) * bool ) list)
        : int =
      match zib_items with
        | [] -> ( -1)
        | hd :: tl -> let (z, _, b) = hd in
           if b 
           then z
           else get_first_selected_z tl             

    (* separate selected and not selected items,
       assign z numbers continuosly*)
    let rec separate_selected
        (acc  : (int * (Dom_html.element Js.t) * bool ) list)
        (is_selected : bool)
        (z_begin : int)
        (z_end : int)
        (zib_items  : (int * (Dom_html.element Js.t) * bool ) list) =
      match zib_items with
        | [] -> acc
        | hd :: tl -> let (z, _, b) = hd in  
          let acc = if b = is_selected && z_begin >= z && z_end < z
          then hd :: acc
          else acc in
          separate_selected acc is_selected z_begin z_end tl 

   end  

module Selection = struct
  include Selection

  let selectables = [Query Selector.item]

  let validate_start = fun e ->
    match Js.to_string e##._type with
    | "mousedown" ->
      let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce e in
      e##.button = 0
    | _ -> true

  let make handle_selected elt =
    let boundaries = [Node elt] in
    make ~validate_start
      ~selectables
      ~boundaries
      ~start_areas:boundaries
      ()
end


let aspect_of_wm_position (p : Wm.position) =
  Utils.resolution_to_aspect ((p.right - p.left), (p.bottom - p.top))

class t
    ~(items : Resizable.t list)
    ~(parent : Dom_html.element Js.t)
    ~(list_of_widgets : List_of_widgets.t)
    (container : Wm.container)
    (scaffold : Scaffold.t)
    elt
    () =
  object(self)

    inherit Drop_target.t elt () as super

    val aspect =
      let w, h = aspect_of_wm_position container.position in
      float_of_int w /. float_of_int h
    val grid_overlay = match Element.query_selector elt Selector.grid_overlay with
      | None -> failwith "widget-editor: grid overlay element not found"
      | Some x ->
        let show_grid_lines = Storage.(get_bool ~default:true show_grid_lines) in
        let show_snap_lines = Storage.(get_bool ~default:true show_snap_lines) in
        Grid_overlay.attach ~show_grid_lines ~show_snap_lines ~size:10 x
    val ghost = match Element.query_selector elt Selector.grid_ghost with
      | None -> failwith "widget-editor: grid ghost element not found"
      | Some x -> x
    val undo_manager = Undo_manager.create ()

    val mutable parent_position = container.position
    val mutable parent_aspect = aspect_of_wm_position container.position
    val mutable format = List_of_widgets.format
    val mutable _items = items
    val mutable _listeners = []
    val mutable _focused_item = None
    val mutable min_size = 20

    val mutable _selection = None

    val mutable _basic_actions = []
    val mutable _widget_selected_actions = []

    method! init () : unit =
      super#init ();
      _selection <- Some (Selection.make (fun _ -> ()) parent);
      _basic_actions <- self#create_actions ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ Resizable.Event.inputs super#root self#handle_item_action
          ; Resizable.Event.changes super#root self#handle_item_change
          ; Resizable.Event.selects super#root self#handle_item_selected
          ; keydowns super#root self#handle_keydown
          ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
      List.iter Widget.destroy _items;
      _items <- [];
      Utils.Option.iter Widget.destroy _selection;
      _selection <- None;
      super#destroy ()

    method! layout () : unit =
      self#fit ();
      List.iter Widget.layout _items;
      grid_overlay#layout ();
      super#layout ()

    method items : Dom_html.element Js.t list =
      self#items_ ()

    method notify : event -> unit = function
      | `Container x ->
        (* TODO add notification if widget layout is changed
           & we have some unsaved work *)
        (* TODO implement simple update *)
        ()

    method value : Wm.container =
      let widgets = List.map (fun x ->
          Widget_utils.widget_of_element
            ~parent_position
            x) self#items in
      { container with widgets }

    method fit () : unit =
      let w = container.position.right - container.position.left in
      let h = container.position.bottom - container.position.top in
      let scale_factor = self#scale_factor in
      let width' = int_of_float @@ float_of_int w *. scale_factor in
      let height' = int_of_float @@ float_of_int h *. scale_factor in
      super#root##.style##.width := Utils.px_js width';
      super#root##.style##.height := Utils.px_js height';
      List.iter (fun item ->
          let pos = Widget_utils.Attr.get_position ~parent_position item in
          let w = float_of_int @@ pos.right - pos.left in
          let h = float_of_int @@ pos.bottom - pos.top in
          let new_w, new_h =
            let w' = w *. scale_factor in
            (* XXX maybe use item aspect ratio to calculate new height? *)
            let h' = h *. scale_factor in
            w', h' in
          let new_left = (float_of_int pos.left *. new_w) /. w in
          let new_top = (float_of_int pos.top *. new_h) /. h in
          item##.style##.top := Utils.px_js @@ Float.to_int @@ Float.floor new_top;
          item##.style##.left := Utils.px_js @@ Float.to_int @@ Float.floor new_left;
          item##.style##.width := Utils.px_js @@ Float.to_int @@ Float.floor new_w;
          item##.style##.height := Utils.px_js @@ Float.to_int @@ Float.floor new_h)
      @@ self#items_ ()

    method actions : Widget.t list =
      _basic_actions

    (* Private methods *)

    method private add_item_ id item (position : Position.t) =
      list_of_widgets#remove_by_id id;
      Dom.appendChild super#root item;
      Position.apply_to_element position item;
      self#set_position_attributes item position

    (** Add item with undo *)
    method private add_item w p =
      let item = make_item ~parent_position ~parent_aspect w in
      self#add_item_ (fst w) item#root p;
      Undo_manager.add undo_manager
        { undo = (fun () -> self#remove_item_ item#root)
        ; redo = (fun () -> self#add_item_ (fst w) item#root p)
        }

    method private remove_item_ (item : Dom_html.element Js.t) =
      list_of_widgets#append_item
      @@ Widget_utils.widget_of_element ~parent_position item;
      Element.remove_child_safe super#root item;
      _items <- List.filter (fun (x : Resizable.t) ->
          let b = Element.equal item x#root in
          if b then x#destroy ();
          not b) _items

    (** Remove item with undo *)
    method private remove_item item =
      let id = Js.to_string item##.id in
      let position = Position.of_element item in
      self#remove_item_ item;
      Undo_manager.add undo_manager
        { undo = (fun () -> self#add_item_ id item position)
        ; redo = (fun () -> self#remove_item_ item)
        }

    method private create_actions () : Widget.t list =
      let menu = Actions.make_overflow_menu
          (fun () -> self#selected)
          Actions.[ undo undo_manager
                  ; redo undo_manager
                  ; add_widget scaffold
                  ] in
      [menu#widget]

    method private selected = []

    method private items_ ?(sort = false) () : Dom_html.element Js.t list =
      let get_position = Widget_utils.Attr.get_position
          ~parent_position
          ~parent_aspect in
      let items = Element.query_selector_all super#root Selector.item in
      if sort
      then
        List.sort (fun x y ->
            let pos_x, pos_y = get_position x, get_position y in
            compare_pair compare compare
              (pos_x.left, pos_x.top)
              (pos_y.left, pos_y.top))
          items
      else items

    method private handle_keydown e _ =
      (* TODO Implement as described in
         https://www.w3.org/TR/wai-aria-practices/#layoutGrid *)
      Js.Opt.iter Dom_html.document##.activeElement (fun active ->
          let items = self#items_ ~sort:true () in
          match Dom_html.Keyboard_code.of_event e with
          (* Navigation keys *)
          | ArrowLeft -> ()
          | ArrowRight -> ()
          | ArrowDown -> ()
          | ArrowUp -> ()
          | PageUp -> () (* XXX optional *)
          | PageDown -> () (* XXX optional *)
          | Home -> ()
          | End -> ()
          (* Other keys *)
          | Enter | Space -> () (* XXX maybe move to the next layer here? *)
          | Delete ->
            self#remove_item active;
            (match items with
             | hd :: _ ->
               set_tab_index ~prev:active (Lazy.from_val items) hd;
               _focused_item <- Some hd
             | _ -> ())
          | _ -> ());
      Lwt.return_unit

    method private handle_item_selected e _ =
      let target = Dom_html.eventTarget e in
      target##focus;
      set_tab_index ?prev:_focused_item (Lazy.from_fun self#items_) target;
      _focused_item <- Some target;
      Lwt.async (fun () ->
          Events.blur target
          >>= fun _ -> (* TODO do smth *) Lwt.return_unit);
      Lwt.return_unit

    method private handle_item_action e _ =
      let target = Dom_html.eventTarget e in
      (match _focused_item with
       | None -> ()
       | Some x -> if not @@ Element.equal x target then x##blur);
      let detail = Widget.event_detail e in
      let position = Position.of_client_rect detail##.rect in
      let original_position = Position.of_client_rect detail##.originalRect in
      let adjusted, lines =
        Position.adjust
          ?aspect_ratio:(Widget_utils.Attr.get_aspect target)
          ~min_width:min_size
          ~min_height:min_size
          ~snap_lines:grid_overlay#snap_lines_visible
          ~action:(match detail##.action with
              | Move -> `Move
              | Resize -> `Resize detail##.direction)
          ~position
          ~original_position
          ~siblings:self#items
          ~parent_size:self#size
          target
      in
      grid_overlay#set_snap_lines lines;
      Position.apply_to_element adjusted target;
      Lwt.return_unit

    (* TODO this is a next task *)
    method private handle_item_change e _ =
      let target = Dom_html.eventTarget e in
      grid_overlay#set_snap_lines [];
      self#set_position_attributes target
        (Position.of_client_rect @@ Widget.event_detail e);
      Lwt.return_unit

    method private set_position_attributes
        (elt : Dom_html.element Js.t)
        (pos : Position.t) =
      (* TODO Implement me! *)
      ()

    method private parent_rect : float * float * float =
      Js.Opt.case (Element.get_parent super#root)
        (fun () -> 0., 0., 1.)
        (fun x ->
           let width = float_of_int x##.offsetWidth in
           let height = float_of_int x##.offsetHeight in
           width, height, width /. height)

    method private scale_factor : float =
      let cur_width, cur_height, cur_aspect = self#parent_rect in
      let w = container.position.right - container.position.left in
      let h = container.position.bottom - container.position.top in
      if cur_aspect > aspect
      then cur_height /. float_of_int h
      else cur_width /. float_of_int w

    method private handle_dropped_json (json : Yojson.Safe.t) : unit Lwt.t =
      let of_yojson = function
        | `List [`String id; json] ->
          begin match Wm.widget_of_yojson json with
            | Ok x -> id, x
            | Error e -> failwith e
          end
        | _ -> failwith "failed to parse json" in
      self#add_item (of_yojson json) (Position.of_element ghost);
      grid_overlay#set_snap_lines [];
      Lwt.return_unit

    method private move_ghost :
      'a. ?aspect:int * int -> (#Dom_html.event as 'a) Js.t -> unit =
      fun ?aspect event ->
      (* FIXME too expensive to call getBoundingClientRect every time *)
      let rect = super#root##getBoundingClientRect in
      let (x, y) = Resizable.get_cursor_position event in
      let point = x -. rect##.left, y -. rect##.top in
      let position =
        { Position.
          x = fst point
        ; y = snd point
        ; w = 100. (* FIXME *)
        ; h = 100. (* FIXME *)
        } in
      Dom.preventDefault event;
      let adjusted, lines =
        Position.adjust
          ?aspect_ratio:None
          ~min_width:min_size
          ~min_height:min_size
          ~snap_lines:grid_overlay#snap_lines_visible
          ~action:`Move
          ~position
          ~original_position:position
          ~siblings:self#items
          ~parent_size:self#size
          ghost
      in
      grid_overlay#set_snap_lines lines;
      Position.apply_to_element adjusted ghost

    method private bring_to_front (items : Dom_html.element Js.t list) : unit =
      (* TODO implement. Should set z-indexes of the provided elements higher
         than indexes of other colliding elements *)
      let open Tmp_bring_to in
      let z_selected_items = get_z_list [] items in
      let all_items = get_all_items in
      let zib_all_list = create_all_z_list [] all_items z_selected_items in
      let zib_all_list_sorted = List.sort 
        (fun 
        (x :(int * (Dom_html.element Js.t) * bool ))
        (y :(int * (Dom_html.element Js.t) * bool )) -> 
        let (z1, _, _) = x in
        let (z2, _, _) = y in
        if z1 = z2 then 0 else
          if z1 < z2 then -1 else 1
        ) zib_all_list in
      let zib_all_list_packed = pack_list 1 [] zib_all_list_sorted in
      let upper_selected_z = get_upper_selected_z 
        1 (List.length z_selected_items) zib_all_list_packed in
      let insert_position_z = upper_selected_z + 1 - (List.length z_selected_items) in
      let all_zib_list_result =
        if insert_position_z <= 0 || insert_position_z > (List.length zib_all_list_packed)
        then zib_all_list_packed
        else 
        let zib_non_selected_begin = separate_selected 
          [] false 1 upper_selected_z zib_all_list_packed in
        let zib_selected = separate_selected 
          [] true 1 (List.length zib_all_list_packed) zib_all_list_packed in
        let zib_non_selected_end = separate_selected 
          [] false 
          (upper_selected_z + 1) (List.length zib_all_list_packed) 
          zib_all_list_packed in
        pack_list 1 []
         (List.append zib_non_selected_begin (List.append zib_selected zib_non_selected_end))
         in
       let _ = set_all_z all_zib_list_result in
      ()
                                
    method private send_to_back (items : Dom_html.element Js.t list) : unit =
      (* TODO implement. Should set z-indexes of the provided elements lower
         than indexes of other colliding elements *)
         let open Tmp_bring_to in
         let z_selected_items = get_z_list [] items in
         let all_items = get_all_items in
         let zib_all_list = create_all_z_list [] all_items z_selected_items in
         let zib_all_list_sorted = List.sort 
           (fun 
           (x :(int * (Dom_html.element Js.t) * bool ))
           (y :(int * (Dom_html.element Js.t) * bool )) -> 
           let (z1, _, _) = x in
           let (z2, _, _) = y in
           if z1 = z2 then 0 else
             if z1 < z2 then -1 else 1
           ) zib_all_list in
         let zib_all_list_packed = pack_list 1 [] zib_all_list_sorted in
         let first_selected_z = get_first_selected_z zib_all_list_packed in
         let insert_position_z = first_selected_z - 1 in
         let all_zib_list_result =
           if insert_position_z <= 0 || insert_position_z > (List.length zib_all_list_packed)
           then zib_all_list_packed
           else 
           let zib_non_selected_begin = separate_selected 
             [] false 1 first_selected_z zib_all_list_packed in
           let zib_selected = separate_selected 
             [] true 1 (List.length zib_all_list_packed) zib_all_list_packed in
           let zib_non_selected_end = separate_selected 
             [] false 
             (first_selected_z + 1) (List.length zib_all_list_packed) 
             zib_all_list_packed in
           pack_list 1 []
            (List.append zib_non_selected_begin (List.append zib_selected zib_non_selected_end))
            in
          let _ = set_all_z all_zib_list_result in
      ()

  end

let make ~(scaffold : Scaffold.t)
    ~(list_of_widgets : List_of_widgets.t)
    (parent : Dom_html.element Js.t)
    (container : Wm.container) =
  let items = List.map (fun x ->
      make_item
        ~parent_aspect:(aspect_of_wm_position container.position)
        ~parent_position:container.position
        x) container.widgets in
  let content =
    Markup.create_grid_overlay ()
    :: Markup.create_grid_ghost ()
    :: List.map Widget.to_markup items in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create_grid ~content () in
  new t ~items ~parent ~list_of_widgets container scaffold elt ()
