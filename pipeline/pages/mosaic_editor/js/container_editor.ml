open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
open Components
open Pipeline_types
open Resizable_grid_utils

(* TODO
   [ ] undo/redo
   [X] container selection (single/multi with selection area)
   [ ] widget selection
   [X] editing mode selection: table, containers
   [X] overflow menu
   [X] contextual actions
   [ ] keyboard navigation for widgets/containers
   [X] add description to tiles
   [ ] add switching between widget/container mode
   [ ] simple wizard
   [ ] `add` widget ui
   [ ] `add` container ui (predefined)
   [ ] container table split ui
   [ ] migrate to 'selection' module in widget editor
   [ ] Take aspect ratio into account (if any) while resizing widget
   [X] Respect parent boundaries when resizing/moving widgets
   [X] Show helper alignment lines in widgets editor
   [X] Stick widget to neighbour elements
   [ ] Extend widget `resize` dir to handle Top, Left, Right, Bottom dirs
*)

include Page_mosaic_editor_tyxml.Container_editor
module Markup = Markup.Container_editor

type editing_mode = Content | Table

let editing_mode_of_enum = function
  | 0 -> Content
  | 1 -> Table
  | _ -> invalid_arg "invalid mode value"

type event =
  [ `Layout of Wm.t
  ]

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

let flip f x y = f y x

module Selector = struct
  let heading = Printf.sprintf ".%s" Card.CSS.primary
  let content = Printf.sprintf ".%s" Card.CSS.media
  let actions = Printf.sprintf ".%s" Card.CSS.actions
  let ar_sizer = Printf.sprintf ".%s" CSS.aspect_ratio_sizer
  let cell = Printf.sprintf ".%s" Resizable_grid.CSS.cell
  let grid = Printf.sprintf ".%s" Resizable_grid.CSS.root
  let mode_switch = Printf.sprintf ".%s" CSS.mode_switch
end

module Selection = struct
  include Selection

  let class_ = Resizable_grid.CSS.cell_selected

  let selectables = [Query Selector.cell]

  let validate_start = fun e ->
    match Js.to_string e##._type with
    | "mousedown" ->
      let (e : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce e in
      e##.button = 0
    | _ -> true

  let on_start = fun { selected; selection; _ } ->
    List.iter (fun x -> Element.remove_class x class_) selected;
    selection#deselect_all ()

  let on_move = fun { selected; removed; _ } ->
    List.iter (fun x -> Element.add_class x class_) selected;
    List.iter (fun x -> Element.remove_class x class_) removed

  let on_stop handle_selected = fun { selection; selected; _ } ->
    selection#keep_selection ();
    handle_selected selection#selected

  let on_select handle_selected = fun item { selection; _ } ->
    let is_selected = Element.has_class item class_ in
    List.iter (fun x -> Element.remove_class x class_) selection#selected;
    (match List.length selection#selected > 1
           && List.memq item selection#selected with
    | true ->
      selection#deselect_all ();
      selection#keep_selection ();
      Element.add_class item class_
    | _ ->
      selection#deselect_all ();
      if is_selected
      then (Element.remove_class item class_; selection#deselect item)
      else (Element.add_class item class_; selection#keep_selection ()));
    handle_selected selection#selected

  let make handle_selected elt =
    let boundaries = [Node elt] in
    Selection.make ~validate_start
      ~selectables
      ~boundaries
      ~start_areas:boundaries
      ~on_start
      ~on_move
      ~on_stop:(on_stop handle_selected)
      ~on_select:(on_select handle_selected)
      ()

end

let swap (a : Dom_html.element Js.t as 'a) (b : 'a) : unit =
  let get_attr attr e =
    Js.Opt.get
      (e##getAttribute (Js.string attr))
      (fun () -> Js.string "") in
  let swap_class e =
    if Element.has_class a Selection.class_
    then (Element.add_class b Selection.class_;
          Element.remove_class a Selection.class_) in
  let id, title, html =
    a##.id
  , get_attr Attr.title a
  , a##.innerHTML in
  a##.id := b##.id;
  a##setAttribute (Js.string Attr.title) (get_attr Attr.title b);
  a##.innerHTML := b##.innerHTML;
  b##.id := id;
  b##setAttribute (Js.string Attr.title) title;
  b##.innerHTML := html;
  swap_class a;
  swap_class b

let cell_title (i : int) =
  Printf.sprintf "Контейнер #%d" i

let gen_cell_title (cells : Dom_html.element Js.t list) =
  (* FIXME implement *)
  let idx = List.length cells in
  cell_title idx

let on_cell_insert
    (grid : Resizable_grid.t)
    (cell : Dom_html.element Js.t) =
  let title = gen_cell_title (grid#cells ()) in
  set_cell_title cell title

(** Updates properties of the aspect ratio sizer according to the new
    area resolution *)
let update_ar_sizer ~width ~height (sizer : Dom_html.element Js.t) =
  let viewbox = Printf.sprintf "0 0 %d %d" width height in
  Element.set_attribute sizer "viewBox" viewbox

let make_icon path = Icon.SVG.(make_simple path)#root

let make_description_dialog () =
  let input =
    Textfield.make_textfield
      ~label:"Наименование"
      Text in
  let title = Dialog.Markup.create_title_simple "Описание" () in
  let content = Dialog.Markup.create_content [input#markup] () in
  let actions = Dialog.Markup.(
      [ create_action ~label:"Отмена" ~action:Close ()
      ; create_action ~label:"ОК" ~action:Accept ()
      ]) in
  input, Dialog.make ~title ~content ~actions ()

let make_undo_manager () =
  let undo_manager = Undo_manager.create () in
  let undo = Icon_button.make
      ~icon:(make_icon Icon.SVG.Path.undo)
      ~on_click:(fun _ _ ->
          Undo_manager.undo undo_manager;
          Lwt.return_unit)
      () in
  let redo = Icon_button.make
      ~icon:(make_icon Icon.SVG.Path.redo)
      ~on_click:(fun _ _ ->
          Undo_manager.redo undo_manager;
          Lwt.return_unit)
      () in
  Undo_manager.set_callback undo_manager (fun m ->
      redo#set_disabled (not @@ Undo_manager.has_redo m);
      undo#set_disabled (not @@ Undo_manager.has_undo m));
  undo#set_disabled true;
  redo#set_disabled true;
  undo, redo, undo_manager

let get f l =
  let rec aux acc = function
    | [] -> None, l
    | x :: tl ->
      if f x
      then Some x, (List.rev acc) @ tl
      else aux (x :: acc) tl in
  aux [] l

class t ?(containers = [])
    ~resolution
    ~(scaffold : Scaffold.t)
    elt () =
  let undo, redo, undo_manager = make_undo_manager () in
  object(self)
    val heading = match Element.query_selector elt Selector.heading with
      | None -> failwith "container-editor: heading element not found"
      | Some x -> x
    val content = match Element.query_selector elt Selector.content with
      | None -> failwith "container-editor: content element not found"
      | Some x -> x
    val actions = match Element.query_selector elt Selector.actions with
      | None -> failwith "container-editor: actions element not found"
      | Some x -> x
    val ar_sizer = match Element.query_selector elt Selector.ar_sizer with
      | None -> failwith "container-editor: aspect ratio sizer element not found"
      | Some x -> x
    val grid : Resizable_grid.t =
      match Element.query_selector elt Selector.grid with
      | None -> failwith "container-editor: grid element not found"
      | Some x -> Resizable_grid.attach
                    ~on_cell_insert
                    ~drag_interval:(Fr 0.05)
                    x

    val description_dialog = make_description_dialog ()

    val mutable _containers : (string * Wm.container) list = containers
    val mutable _listeners = []
    val mutable _content_listeners = []
    val mutable _drag_target = Js.null

    val mutable _selection = None
    val mutable _resize_observer = None
    val mutable _top_app_bar_context = []
    val mutable _edit_mode = Table

    val mutable _basic_actions = None
    val mutable _cell_selected_actions = []
    val mutable _cont_selected_actions = []

    val mutable _widget_editor = None
    val mutable _mode_switch = None

    inherit Widget.t elt () as super

    method! init () : unit =
      _resize_observer <- Some (self#create_resize_observer ());
      _selection <- Some (Selection.make self#handle_selected content);
      let basic_actions = self#create_actions () in
      _basic_actions <- Some basic_actions;
      _cell_selected_actions <- self#create_cell_selected_actions ();
      _cont_selected_actions <- self#create_cont_selected_actions ();
      _mode_switch <- begin match Element.query_selector elt Selector.mode_switch with
        | None -> failwith "container-editor: mode switch element not found"
        | Some x ->
          let on_change = function
            | None -> assert false
            | Some x -> self#switch_mode (editing_mode_of_enum x) in
          let tab_bar = Tab_bar.attach
              ~on_change:(fun _ x ->
                  on_change x#active_tab_index;
                  Lwt.return_unit)
              x in
          on_change tab_bar#active_tab_index;
          Some tab_bar
      end;
      (match scaffold#top_app_bar with
       | None -> ()
       | Some x -> x#set_actions @@ List.map Widget.root [basic_actions]);
      (* FIXME *)
      List.iter (Element.append_child actions % Widget.root) @@ self#create_grid_actions ();
      Dom.appendChild Dom_html.document##.body (snd description_dialog)#root;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ keydowns super#root self#handle_keydown
          ; dragstarts grid#root self#handle_dragstart
          ; dragends grid#root self#handle_dragend
          ; dragenters grid#root self#handle_dragenter
          ; dragleaves grid#root self#handle_dragleave
          ; dragovers grid#root self#handle_dragover
          ; drops grid#root self#handle_drop
          ]);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      let parent_h = content##.offsetHeight in
      let width = parent_h * (fst resolution) / (snd resolution) in
      grid#root##.style##.width := Utils.px_js width;
      grid#root##.style##.height := Utils.px_js parent_h;
      Utils.Option.iter (Widget.layout % snd) _widget_editor;
      Utils.Option.iter Widget.layout _basic_actions;
      List.iter Widget.layout _cell_selected_actions;
      super#layout ()

    method! destroy () : unit =
      Utils.Option.iter Ui_templates.Resize_observer.disconnect _resize_observer;
      _resize_observer <- None;
      Utils.Option.iter (fun x -> x#destroy ()) _selection;
      _selection <- None;
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
      Dom.removeChild Dom_html.document##.body (snd description_dialog)#root;
      super#destroy ()

    method selected : Dom_html.element Js.t list =
      self#selection#selected

    method clear_selection () : unit =
      List.iter (flip Element.remove_class Selection.class_) self#selected;
      self#selection#deselect_all ()

    method resolution : int * int =
      resolution

    (* TODO implement *)
    method value : Wm.t =
      let cols = grid#cols in
      let rows = grid#rows in
      let get_cell_len_in_frs
        (pos_stop : int)
        (cols_or_rows : Resizable_grid.value array)
        : float = 
        let l = ref 0.0 in
        for i = 0 to (Array.length cols) - 1 do
          let v = match cols_or_rows.(i) with
            | Auto -> 0.0
            | Px v -> 0.0
            | Fr v -> v
            | Pc v -> 0.0
          in
          if (pos_stop <= i + 1)
          then l := !l +. v
          else l := !l 
        done;
        !l
        in 
      let cells = grid#cells () in
      let (cell_pos_list : cell_position list) =
        List.map (fun (cell : Dom_html.element Js.t) ->
          let pos = get_cell_position cell in pos) cells in
      let rec create_out_position_list
        (acc : Pipeline_types.Wm.position list)
        (cols : Resizable_grid.value array)
        (rows : Resizable_grid.value array)
        (resolution : (int * int)) 
        = function
          | [] -> acc
          | hd :: tl -> 
            let acc =             
              ({ left = (int_of_float ((get_cell_len_in_frs hd.col cols) *. 10000.0)) * (fst resolution) / 10000
               ; top = (int_of_float ((get_cell_len_in_frs hd.row rows) *. 10000.0)) * (snd resolution) / 10000
               ; right = (int_of_float ((get_cell_len_in_frs (hd.col + hd.col_span) cols) *. 10000.0)) * (fst resolution) / 10000
               ; bottom = (int_of_float ((get_cell_len_in_frs (hd.row + hd.row_span) rows) *. 10000.0)) * (snd resolution) / 10000
               }:Pipeline_types.Wm.position)
               :: acc 
             in
             create_out_position_list acc cols rows resolution tl
      in     
      let (positions : Pipeline_types.Wm.position list) = 
       create_out_position_list [] cols rows self#resolution cell_pos_list in
      let rec create_containers 
        (acc : Pipeline_types.Wm.container list) = function
          | [] -> acc
          | hd :: tl -> 
            let acc =
              ({ position = hd
               ; widgets = []
               }:Pipeline_types.Wm.container)
               :: acc
               in
               create_containers acc tl
        in
      let (containers : Pipeline_types.Wm.container list) = create_containers [] positions in
      let (names : string list) =
          List.map (fun (cell : Dom_html.element Js.t) ->
            let pos = get_cell_title cell in pos) cells in
      { resolution = self#resolution
      ; widgets = []
      ; layout = (List.combine names containers)
      }
      
    (* TODO implement layout update *)
    method notify : event -> unit = function
      | `Layout wm ->
        match _widget_editor with
        | None -> ()
        | Some (id, editor) ->
          match List.assoc_opt id wm.layout with
          | None -> () (* FIXME container lost, handle it somehow *)
          | Some x -> editor#notify @@ `Container x

    method edit_container (container : Dom_html.element Js.t) : unit Lwt.t =
      let id = get_cell_title container in
      let widgets = Wm_widget.elements container in
      let cont = Container.of_element container in
      let editor = Widget_editor.make cont in
      Utils.Option.iter (Dom.removeChild heading % Widget.root) _mode_switch;
      Dom.removeChild content grid#root;
      Dom.appendChild content editor#root;
      _widget_editor <- Some (id, editor);
      let width = cont.position.right - cont.position.left in
      let height = cont.position.bottom - cont.position.top in
      (* Set aspect ratio sizer for container dimensions *)
      update_ar_sizer ~width ~height ar_sizer;
      editor#layout ();
      (* Thread which is resolved when we're back to the container mode *)
      let t, w = Lwt.wait () in
      scaffold#set_on_navigation_icon_click (fun _ _ ->
          Lwt.wakeup_later w ();
          Lwt.return_unit);
      t
      >>= fun () ->
      self#update_widget_elements editor#value.widgets widgets container;
      Dom.removeChild content editor#root;
      Dom.appendChild content grid#root;
      Utils.Option.iter (Dom.appendChild heading % Widget.root) _mode_switch;
      grid#layout ();
      editor#destroy ();
      (* Restore aspect ratio sizer dimensions *)
      let width, height = self#resolution in
      update_ar_sizer ~width ~height ar_sizer;
      _widget_editor <- None;
      Lwt.return_unit

    (* Private methods *)

    method private handle_dragstart e _ =
      let target = Dom_html.eventTarget e in
      _drag_target <- e##.target;
      e##.dataTransfer##setData (Js.string "text/html") target##.innerHTML;
      e##.dataTransfer##.effectAllowed := Js.string "move";
      Element.add_class target CSS.cell_dragging;
      Lwt.return_unit

    method private handle_dragenter e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Resizable_grid.CSS.cell
      && (Js.Opt.case _drag_target (fun () -> true) (not % Element.equal target))
      then Element.add_class target CSS.cell_dragover;
      Lwt.return_unit

    method private handle_dragleave e _ =
      let target = Dom_html.eventTarget e in
      Element.remove_class target CSS.cell_dragover;
      Lwt.return_unit

    method private handle_dragover e _ =
      let target = Dom_html.eventTarget e in
      if Element.has_class target Resizable_grid.CSS.cell
      && (not (Js.some target == _drag_target))
      then (Dom.preventDefault e;
            e##.dataTransfer##.dropEffect := Js.string "move");
      Lwt.return_unit

    method private handle_drop e _ =
      Dom_html.stopPropagation e;
      let target = Dom_html.eventTarget e in
      Js.Opt.iter _drag_target (fun dragged ->
          if not @@ Element.equal dragged target
          then (
            swap dragged target;
            let action =
              { Undo_manager.
                undo = (fun () -> swap target dragged)
              ; redo = (fun () -> swap dragged target)
              } in
            Undo_manager.add undo_manager action));
      Lwt.return_unit

    method private handle_dragend e _ =
      let target = Dom_html.eventTarget e in
      Element.remove_class target CSS.cell_dragging;
      List.iter (flip Element.remove_class CSS.cell_dragover) @@ grid#cells ();
      Lwt.return_unit

    (* TODO implement *)
    method private handle_keydown e _ : unit Lwt.t =
      (match Dom_html.Keyboard_code.of_event e with
       | Escape -> begin match self#selected with
           | [] -> ()
           | _ -> self#clear_selection ()
         end
       | _ -> ());
      Lwt.return_unit

    method private handle_click e _ : unit Lwt.t =
      match Resizable_grid_utils.cell_of_event (grid#cells ()) e with
      | None -> Lwt.return_unit
      | Some cell ->
        let is_selected = List.memq cell self#selected in
        self#clear_selection ();
        if is_selected
        then (self#selection#deselect cell;
              Element.remove_class cell Selection.class_)
        else (self#selection#select [cell];
              Element.add_class cell Selection.class_);
        self#handle_selected self#selected;
        Lwt.return_unit

    method private restore_top_app_bar_context () : unit =
      match _top_app_bar_context with
      | [] -> ()
      | f :: tl -> f (); _top_app_bar_context <- tl

    method private handle_selected = function
      | [] -> self#restore_top_app_bar_context ()
      | cells ->
        let title = match _edit_mode, cells with
          | Content, [cell] ->
            (match Element.get_attribute cell Attr.title with
             | None -> "Выбран контейнер"
             | Some x -> x)
          | _ ->
            Printf.sprintf "Выбрано ячеек: %d"
            @@ List.length cells in
        let restore = Container_editor_actions.transform_top_app_bar
            ~class_:Page_mosaic_editor_tyxml.CSS.top_app_bar_contextual
            ~actions:(match _edit_mode with
                | Table -> _cell_selected_actions
                | Content -> _cont_selected_actions)
            ~title
            scaffold in
        match _top_app_bar_context with
        | _ :: _ -> ()
        | [] ->
          scaffold#set_on_navigation_icon_click (fun _ _ ->
              self#clear_selection ();
              self#restore_top_app_bar_context ();
              Lwt.return_unit);
          _top_app_bar_context <- [restore]

    method private create_actions () : Overflow_menu.t =
      let open Container_editor_actions in
      make_overflow_menu (fun () -> self#selected) scaffold [wizard grid]

    (* TODO consider inner grids *)
    method private create_cell_selected_actions () : Widget.t list =
      let open Container_editor_actions in
      let f () =
        self#clear_selection ();
        self#restore_top_app_bar_context () in
      let menu = make_overflow_menu
          (fun () -> self#selected)
          scaffold
          [ merge ~f undo_manager grid
          ; add_row_above grid
          ; add_row_below grid
          ; remove_row ~f grid
          ; add_col_left grid
          ; add_col_right grid
          ; remove_col ~f grid
          ] in
      [menu#widget]

    method private create_cont_selected_actions () : Widget.t list =
      let open Container_editor_actions in
      let menu = make_overflow_menu
          (fun () -> self#selected)
          scaffold
          [ edit self#edit_container
          ; description description_dialog
          ] in
      [menu#widget]

    method private create_grid_actions () =
      let icons = Card.Actions.make_icons [undo; redo] in
      let submit = Button.make
          ~label:"Применить"
          ~on_click:(fun _ _ _ ->
              let open Resizable_grid in
              print_endline
              @@ Printf.sprintf "cols: %s, rows: %s"
                (String.concat " " @@ List.map value_to_string @@ Array.to_list grid#cols)
                (String.concat " " @@ List.map value_to_string @@ Array.to_list grid#rows);
              Lwt.return_unit)
          () in
      let buttons = Card.Actions.make_buttons [submit] in
      [icons; buttons]

    method private create_resize_observer () =
      let f = fun _ -> self#layout () in
      Ui_templates.Resize_observer.observe ~f ~node:super#root ()

    method private selection : Selection.t =
      Utils.Option.get _selection

    method private switch_mode (mode : editing_mode) : unit =
      self#clear_selection ();
      self#restore_top_app_bar_context ();
      _edit_mode <- mode;
      match mode with
      | Table -> self#set_table_mode ()
      | Content -> self#set_content_mode ()

    method private set_content_mode () : unit =
      self#selection#set_disabled true;
      super#add_class CSS.content_mode;
      _content_listeners <- Events.(
          [ clicks grid#root self#handle_click
          ])

    method private set_table_mode () : unit =
      self#selection#set_disabled false;
      super#remove_class CSS.content_mode;
      List.iter Lwt.cancel _content_listeners;
      _content_listeners <- []

    method private update_widget_elements
        (widgets : (string * Wm.widget) list)
        (elements : Dom_html.element Js.t list)
        (container : Dom_html.element Js.t) : unit =
      let rest =
        List.fold_left (fun acc (elt : Dom_html.element Js.t) ->
            let id = Js.to_string elt##.id in
            let w, rest = get (String.equal id % fst) acc in
            (match w with
             | Some w -> Wm_widget.update_element elt w
             | None -> Dom.removeChild container elt);
            rest) widgets elements in
      List.iter (Dom.appendChild container % Wm_widget.to_element) rest

  end

let make
    ~(scaffold : Scaffold.t)
    (wm : Wm.t) =
  let cols = 5 in
  let rows = 5 in
  let cells = Resizable_grid_utils.gen_cells
      ~f:(fun ~col ~row () ->
          let idx = ((pred row) * cols) + col in
          let title = cell_title idx in
          Resizable_grid.Markup.create_cell
            ~attrs:[Html.a_user_data "title" title]
            ~col_start:col
            ~row_start:row
            ~content:[]
            ())
      ~cols
      ~rows in
  let grid = Resizable_grid.Markup.create
      ~rows
      ~cols
      ~content:cells
      () in
  let elt =
    To_dom.of_element
    @@ Markup.create
      ~classes:[Card.CSS.root]
      ~width:(fst wm.resolution)
      ~height:(snd wm.resolution)
      ~grid () in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    ~scaffold
    elt ()
