open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types
open Resizable_grid_utils

include Page_mosaic_editor_tyxml.Container_editor
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type editing_mode = Content | Table

let editing_mode_of_enum = function
  | 0 -> Content
  | 1 -> Table
  | _ -> invalid_arg "invalid mode value"

type event =
  [ `Layout of Wm.t
  | `Streams of Structure.packed list
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
  let widget_wrapper = Printf.sprintf ".%s" CSS.widget_wrapper
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

let container_of_element (elt : Dom_html.element Js.t) : Wm.container =
  (* TODO implement. Seems that it is better to use relative
     dimensions like 'fr' rather than absolute pixel values. *)
  let width = elt##.offsetWidth in
  let height = elt##.offsetHeight in
  let position =
    { Wm. left = 0
    ; right = width
    ; top = 0
    ; bottom = height
    } in
  { position
  ; widgets = Wm_widget.of_container elt
  }

let get f l =
  let rec aux acc = function
    | [] -> None, l
    | x :: tl ->
      if f x
      then Some x, (List.rev acc) @ tl
      else aux (x :: acc) tl in
  aux [] l

let init_top_app_bar_icon (scaffold : Scaffold.t) =
  match Utils.Option.bind (fun x -> x#leading) scaffold#top_app_bar with
  | None -> ()
  | Some x ->
    let icons =
      Element.query_selector_all x
      @@ Printf.sprintf ".%s" Icon.CSS.root in
    match icons with
    | [x] -> Element.add_class x CSS.nav_icon_main
    | [x; y] ->
      Element.add_class x CSS.nav_icon_main;
      Element.add_class x CSS.nav_icon_aux
    | _ -> ()

let set_top_app_bar_icon (scaffold : Scaffold.t) typ icon =
  let class_, index = match typ with
    | `Main -> CSS.nav_icon_main, 0
    | `Aux -> CSS.nav_icon_aux, 1 in
  match Utils.Option.bind (fun x -> x#leading) scaffold#top_app_bar with
  | None -> None
  | Some x ->
    let prev =
      Element.query_selector x
      @@ Printf.sprintf ".%s" class_ in
    (match prev with
     | None -> ()
     | Some prev -> Dom.removeChild x prev);
    Element.add_class icon class_;
    Element.insert_child_at_index x index icon;
    prev

type widget_mode_state =
  { icon : Dom_html.element Js.t option
  ; restore : unit -> unit
  ; editor : Widget_editor.t
  ; cell : Dom_html.element Js.t
  ; container : Wm.container
  }

class t ~(scaffold : Scaffold.t)
    (wm : Wm.t)
    (elt : Dom_html.element Js.t)
    () =
  object(self)
    val close_icon = Icon.SVG.(make_simple Path.close)
    val back_icon = Icon.SVG.(make_simple Path.arrow_left)
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
    val undo_manager = Undo_manager.create ()
    val list_of_widgets = List_of_widgets.make Test.widgets (* FIXME *)

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

    val mutable _resolution = wm.resolution
    val mutable _widget_editor = None
    val mutable _mode_switch = None

    inherit Widget.t elt () as super

    method! init () : unit =
      init_top_app_bar_icon scaffold;
      ignore @@ set_top_app_bar_icon scaffold `Aux close_icon#root;
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
      (match scaffold#side_sheet with
       | None -> ()
       | Some x -> Dom.appendChild x#root list_of_widgets#root);
      (match scaffold#top_app_bar with
       | None -> ()
       | Some x -> x#set_actions @@ List.map Widget.root [basic_actions]);
      (* FIXME *)
      List.iter (Element.append_child actions % Widget.root) @@ self#create_grid_actions ();
      Dom.appendChild Dom_html.document##.body (snd description_dialog)#root;
      super#init ()

    method! initial_sync_with_dom () : unit =
      _listeners <- Events.(
          [ dragstarts grid#root self#handle_dragstart
          ; dragends grid#root self#handle_dragend
          ; dragenters grid#root self#handle_dragenter
          ; dragleaves grid#root self#handle_dragleave
          ; dragovers grid#root self#handle_dragover
          ; drops grid#root self#handle_drop
          ]);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      let parent_h = content##.offsetHeight in
      let width = parent_h * (fst _resolution) / (snd _resolution) in
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
      _resolution

    (* TODO implement *)
    method value : Wm.t =
      (* let cols = grid#cols in
       * let rows = grid#rows in
       * let cells = grid#cells () in
       * let _ = List.map (fun (cell : Dom_html.element Js.t) ->
       *     let pos = get_cell_position cell in
       *     ()) cells in *)
      { resolution = self#resolution
      ; widgets = []
      ; layout = []
      }

    (* TODO implement layout update *)
    method notify : event -> unit = function
      | `Streams _ -> ()
      | `Layout wm ->
        match _widget_editor with
        | None -> ()
        | Some (id, editor) ->
          match List.assoc_opt id wm.layout with
          | None -> () (* FIXME container lost, handle it somehow *)
          | Some x -> editor#notify @@ `Container x

    method edit_container (container : Dom_html.element Js.t) : unit Lwt.t =
      self#switch_to_widget_mode container
      >>= self#switch_to_container_mode

    (* Private methods *)

    method private switch_to_container_mode
        ({ restore; icon; editor; cell; container } : widget_mode_state) =
      restore ();
      Utils.Option.iter (ignore % set_top_app_bar_icon scaffold `Main) icon;
      self#update_widget_elements editor#value.widgets container cell;
      Dom.removeChild content editor#root;
      Dom.appendChild content grid#root;
      Utils.Option.iter (Dom.appendChild heading % Widget.root) _mode_switch;
      grid#layout ();
      editor#destroy ();
      (* Restore aspect ratio sizer dimensions *)
      let width, height = self#resolution in
      update_ar_sizer ~width ~height ar_sizer;
      _widget_editor <- None;
      (match scaffold#side_sheet with
       | None -> Lwt.return_unit
       | Some x -> x#toggle ~force:false ())

    method private switch_to_widget_mode (cell : Dom_html.element Js.t) =
      let id = get_cell_title cell in
      let (container : Wm.container) = container_of_element cell in
      let editor = Widget_editor.make
          ~scaffold
          ~list_of_widgets
          content
          container in
      Utils.Option.iter (Dom.removeChild heading % Widget.root) _mode_switch;
      Dom.removeChild content grid#root;
      Dom.appendChild content editor#root;
      _widget_editor <- Some (id, editor);
      let icon = set_top_app_bar_icon scaffold `Main back_icon#root in
      self#restore_top_app_bar_context ();
      let restore = Actions.transform_top_app_bar
          ~title:id
          ~actions:editor#actions
          scaffold in
      (* FIXME switch from pixels to relative coordinates (fr) *)
      let width = container.position.right - container.position.left in
      let height = container.position.bottom - container.position.top in
      (* Set aspect ratio sizer for container dimensions *)
      update_ar_sizer ~width ~height ar_sizer;
      editor#layout ();
      let state = { icon; restore; editor; cell; container } in
      (* Thread which is resolved when we're back to the container mode *)
      let t, w = Lwt.wait () in
      scaffold#set_on_navigation_icon_click (fun _ _ ->
          Lwt.wakeup_later w state;
          Lwt.return_unit);
      t

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
        let restore = Actions.transform_top_app_bar
            ~class_:CSS.top_app_bar_contextual
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
      Actions.make_overflow_menu (fun () -> self#selected)
        Actions.[ undo undo_manager
                ; redo undo_manager
                ; wizard grid ]

    (* TODO consider inner grids *)
    method private create_cell_selected_actions () : Widget.t list =
      let f () =
        self#clear_selection ();
        self#restore_top_app_bar_context () in
      let menu = Actions.make_overflow_menu
          (fun () -> self#selected)
          Actions.[ merge ~f undo_manager grid
                  ; add_row_above grid
                  ; add_row_below grid
                  ; remove_row ~f grid
                  ; add_col_left grid
                  ; add_col_right grid
                  ; remove_col ~f grid
                  ] in
      [menu#widget]

    method private create_cont_selected_actions () : Widget.t list =
      let menu = Actions.make_overflow_menu
          (fun () -> self#selected)
          [ Actions.edit self#edit_container
          ; Actions.description description_dialog
          ] in
      [menu#widget]

    method private create_grid_actions () =
      let submit = Button.make
          ~label:"Применить"
          ~on_click:(fun btn _ _ ->
              let value = self#value in
              let log x : unit = Js.Unsafe.global##.console##log x in
              log @@ Json.unsafe_input @@ Js.string
              @@ Yojson.Safe.to_string @@ Wm.to_yojson value;
              let t = Pipeline_http_js.Http_wm.set_layout value in
              btn#set_loading_lwt t;
              t >>= fun _ -> Lwt.return_unit)
          () in
      let buttons = Card.Actions.make_buttons [submit] in
      [buttons]

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
        (container : Wm.container)
        (cell : Dom_html.element Js.t) : unit =
      match Element.query_selector cell Selector.widget_wrapper with
      | None -> ()
      | Some wrapper ->
        let elements = Wm_widget.elements wrapper in
        let rest =
          List.fold_left (fun acc (elt : Dom_html.element Js.t) ->
              let id = Js.to_string elt##.id in
              let w, rest = get (String.equal id % fst) acc in
              (match w with
               | Some (_, w) ->
                 Wm_widget.apply_to_element
                   ~parent_aspect:(16, 9) (* FIXME *)
                   ~parent_position:container.position
                   elt
                   w
               | None -> Dom.removeChild cell elt);
              rest) widgets elements in
        let make_element w =
          Tyxml_js.To_dom.of_element
          @@ Markup.create_widget w in
        List.iter (Dom.appendChild cell % make_element) rest

  end

type grid_properties =
  { rows : Resizable_grid.value list
  ; cols : Resizable_grid.value list
  ; cells : (string * (Wm.container * Resizable_grid.cell_position)) list
  }

let first_grid_index = 1

let rec get_deltas points =
  List.rev @@ snd @@ List.fold_left (fun (prev, deltas) x ->
      let delta = x - prev in
      (prev + delta, delta :: deltas)) (0, []) points

let points_to_frs resolution deltas : (Resizable_grid.value list) =
  let res = float_of_int resolution in
  let len = float_of_int @@ List.length deltas in
  try List.map (fun x -> Resizable_grid.Fr ((float_of_int x) /. res *. len)) deltas
  with Division_by_zero -> []

let get_cell_pos_part (edge : int) points =
  let rec aux cnt = function
    | [] -> failwith "Corresponding cell edge not found" (* FIXME *)
    | x :: _ when x = edge -> cnt
    | _ :: tl -> aux (cnt + 1) tl in
  if edge = 0 then first_grid_index
  else aux (first_grid_index + 1) points

let get_cell_positions ~lefts ~tops =
  List.map (fun (id, ({ position = x; _ } as c) : string * Wm.container) ->
      let col = get_cell_pos_part x.left lefts in
      let row = get_cell_pos_part x.top tops in
      let col_end = get_cell_pos_part x.right lefts in
      let row_end = get_cell_pos_part x.bottom tops in
      id, (c, { Resizable_grid.
                col
              ; row
              ; col_span = col_end - col
              ; row_span = row_end - row
              }))

let grid_properties_of_layout ({ resolution = w, h; layout; _ } : Wm.t) =
  let sort = List.sort_uniq compare in
  let lefts, tops =
    List.split @@ List.map (fun (_, { position; _ } : _ * Wm.container) ->
        position.right, position.bottom) layout
    |> fun (x, y) -> sort x, sort y in
  let cols = points_to_frs w (get_deltas lefts) in
  let rows = points_to_frs h (get_deltas tops) in
  let cells = get_cell_positions ~lefts ~tops layout in
  { rows; cols; cells }

let content_of_container (container : Wm.container) =
  let widgets = List.map Markup.create_widget container.widgets in
  [Markup.create_widget_wrapper widgets]

let make_grid (props : grid_properties) =
  let cells = List.map (fun (id, (container, pos)) ->
      Resizable_grid.Markup.create_cell
        ~attrs:Tyxml_js.Html.([a_user_data "title" id])
        ~content:(content_of_container container)
        pos)
      props.cells in
  Resizable_grid.Markup.create
    ~rows:(`Value props.rows)
    ~cols:(`Value props.cols)
    ~content:cells
    ()

let make
    ~(scaffold : Scaffold.t)
    (streams : Structure.packed list)
    (wm : Wm.t) =
  let grid = make_grid @@ grid_properties_of_layout wm in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create
      ~width:(float_of_int @@ fst wm.resolution)
      ~height:(float_of_int @@ snd wm.resolution)
      ~grid () in
  new t ~scaffold wm elt ()
